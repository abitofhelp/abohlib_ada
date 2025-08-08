--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Generic_Object_Pool - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Unchecked_Deallocation;
with Ada.Calendar;
with System;

package body Abohlib.Core.Domain.Utilities.Generic_Object_Pool is

   use Abohlib.Core.Domain.Errors.Error_Strings;
   use Abohlib.Core.Domain.Errors.Field_Strings;
   use Abohlib.Core.Domain.Errors.Recovery_Strings;
   use Abohlib.Core.Domain.Errors.Correlation_ID_Strings;
   use Abohlib.Core.Domain.Errors.Context_Strings;

   --  ==========================================================================
   --  Memory Management
   --  ==========================================================================

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Object_Type,
      Name   => Object_Access);

   --  ==========================================================================
   --  Lifecycle Management
   --  ==========================================================================

   overriding procedure Initialize (Pool : in out Object_Pool) is
   begin
      --  Pre-allocate initial objects if requested
      if Initial_Size > 0 then
         declare
            Ignore : constant Count_Result.Result := 
               Preallocate (Pool, Element_Count_Type (Initial_Size));
         begin
            --  Ignore result as this is best-effort initialization
            null;
         end;
      end if;
   end Initialize;

   overriding procedure Finalize (Pool : in out Object_Pool) is
   begin
      --  Free all pooled objects
      for Obj of Pool.Available loop
         declare
            Temp : Object_Access := Obj;
         begin
            Free (Temp);
         end;
      end loop;
      Pool.Available.Clear;
   end Finalize;

   --  ==========================================================================
   --  Pool Operations
   --  ==========================================================================

   function Get
     (Pool : in out Object_Pool) return Pool_Result.Result
   is
   begin
      if not Pool.Available.Is_Empty then
         --  Reuse existing object from pool (fast path)
         declare
            Obj : constant Object_Access := Pool.Available.Last_Element;
         begin
            Pool.Available.Delete_Last;
            return Pool_Result.Ok (Obj);
         end;
      else
         --  Pool is empty, need to allocate
         if Natural (Pool.Allocated_Total) < Max_Size then
            --  Still under limit, allocate and track
            Pool.Allocated_Total := Element_Count_Type (Natural (Pool.Allocated_Total) + 1);
            
            --  Update high water mark
            if Pool.Allocated_Total > Pool.High_Water_Mark then
               Pool.High_Water_Mark := Pool.Allocated_Total;
            end if;
            
            return Pool_Result.Ok (new Object_Type);
         else
            --  Pool exhausted
            return Pool_Result.Err
              (Resource_Error_Type'(
                 Base => Domain_Error'(
                    Category => Resource_Error,
                    Severity => Error,
                    Message => To_Bounded_String
                       ("Object pool exhausted: max size " & Max_Size'Image & " reached"),
                    Occurred_At => Ada.Calendar.Clock,
                    Recovery_Suggestion => To_Bounded_String
                       ("Consider increasing pool size or returning objects sooner"),
                    Correlation_ID => Generate_Correlation_ID,
                    Error_Context => To_Bounded_String ("Generic_Object_Pool.Get"),
                    Source_Location => System.Null_Address),
                 Kind => Exhausted,
                 Resource_Type => To_Bounded_String ("Object_Pool"),
                 Resource_Id => Error_Strings.Null_Bounded_String));
         end if;
      end if;
   end Get;

   procedure Return_To_Pool
     (Pool : in out Object_Pool;
      Obj  : in out Object_Access)
   is
   begin
      if Natural (Pool.Available.Length) < Max_Size then
         --  Reset object state before returning to pool
         Reset_Object (Obj.all);
         
         --  Add back to available pool
         Pool.Available.Append (Obj);
         
         --  Null the caller's access to prevent reuse
         Obj := null;
      else
         --  Pool is full - deallocate instead
         Free (Obj);
      end if;
   end Return_To_Pool;

   --  ==========================================================================
   --  Pool Status Operations
   --  ==========================================================================

   function Available_Count
     (Pool : Object_Pool) return Element_Count_Type
   is
   begin
      return Element_Count_Type (Pool.Available.Length);
   end Available_Count;

   function Allocated_Count
     (Pool : Object_Pool) return Element_Count_Type
   is
   begin
      return Pool.Allocated_Total;
   end Allocated_Count;

   function Has_Available
     (Pool : Object_Pool) return Boolean
   is
   begin
      return not Pool.Available.Is_Empty;
   end Has_Available;

   function Capacity return Element_Count_Type is
   begin
      return Element_Count_Type (Max_Size);
   end Capacity;

   --  ==========================================================================
   --  Pool Management
   --  ==========================================================================

   function Preallocate
     (Pool  : in out Object_Pool;
      Count : Element_Count_Type) return Count_Result.Result
   is
      Allocated : Element_Count_Type := 0;
   begin
      --  Calculate how many we can allocate
      declare
         Current_Total : constant Element_Count_Type := Pool.Allocated_Total;
         Space_Left    : constant Natural := Max_Size - Natural (Current_Total);
         To_Allocate   : constant Natural := Natural'Min (Natural (Count), Space_Left);
      begin
         --  Allocate objects and add to pool
         for I in 1 .. To_Allocate loop
            declare
               New_Obj : constant Object_Access := new Object_Type;
            begin
               Pool.Available.Append (New_Obj);
               Pool.Allocated_Total := Element_Count_Type (Natural (Pool.Allocated_Total) + 1);
               Allocated := Element_Count_Type (Natural (Allocated) + 1);
            end;
         end loop;
         
         --  Update high water mark
         if Pool.Allocated_Total > Pool.High_Water_Mark then
            Pool.High_Water_Mark := Pool.Allocated_Total;
         end if;
         
         if Allocated = Count then
            return Count_Result.Ok (Allocated);
         else
            return Count_Result.Err
              (Resource_Error_Type'(
                 Base => Domain_Error'(
                    Category => Resource_Error,
                    Severity => Warning,
                    Message => To_Bounded_String
                       ("Could only preallocate " & Allocated'Image & 
                        " of requested " & Count'Image & " objects"),
                    Occurred_At => Ada.Calendar.Clock,
                    Recovery_Suggestion => To_Bounded_String
                       ("Pool size limit reached during preallocation"),
                    Correlation_ID => Generate_Correlation_ID,
                    Error_Context => To_Bounded_String ("Generic_Object_Pool.Preallocate"),
                    Source_Location => System.Null_Address),
                 Kind => Exhausted,
                 Resource_Type => To_Bounded_String ("Object_Pool"),
                 Resource_Id => Error_Strings.Null_Bounded_String));
         end if;
      end;
   end Preallocate;

   procedure Clear_Available (Pool : in out Object_Pool) is
   begin
      --  Free all available objects
      for Obj of Pool.Available loop
         declare
            Temp : Object_Access := Obj;
         begin
            Free (Temp);
         end;
      end loop;
      
      --  Clear the vector and update count
      Pool.Allocated_Total := Element_Count_Type (Natural (Pool.Allocated_Total) - Natural (Pool.Available.Length));
      Pool.Available.Clear;
   end Clear_Available;

end Abohlib.Core.Domain.Utilities.Generic_Object_Pool;