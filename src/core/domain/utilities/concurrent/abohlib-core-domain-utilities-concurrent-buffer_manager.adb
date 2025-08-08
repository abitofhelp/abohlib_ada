--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Concurrent.Buffer_Manager - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Unchecked_Deallocation;
with Abohlib.Core.Domain.Errors; use Abohlib.Core.Domain.Errors;

package body Abohlib.Core.Domain.Utilities.Concurrent.Buffer_Manager is

   --  Memory management
   procedure Free_Stream_Element_Array is 
     new Ada.Unchecked_Deallocation (Stream_Element_Array, Stream_Element_Array_Access);

   --  Error creation helpers
   function Make_Buffer_Not_Initialized_Error return Buffer_Error is
   begin
      return Buffer_Error'(
         Base => Make_State_Error(
            Current_State => "Not Initialized",
            Attempted_Transition => "Buffer Operation",
            Message => "Buffer manager not initialized",
            Recovery => "Call Initialize first"
         ).Base
      );
   end Make_Buffer_Not_Initialized_Error;
   
   function Make_Buffer_Shutdown_Error return Buffer_Error is
   begin
      return Buffer_Error'(
         Base => Make_State_Error(
            Current_State => "Shutdown",
            Attempted_Transition => "Buffer Operation",
            Message => "Buffer manager is shut down",
            Recovery => "Initialize a new buffer manager"
         ).Base
      );
   end Make_Buffer_Shutdown_Error;
   
   function Make_No_Free_Buffer_Error return Buffer_Error is
   begin
      return Buffer_Error'(
         Base => Make_Resource_Error(
            Kind => Unavailable,
            Resource_Type => "Buffer",
            Message => "No free buffers available",
            Recovery => "Wait for buffer to be consumed or increase buffer count"
         ).Base
      );
   end Make_No_Free_Buffer_Error;
   
   function Make_No_Ready_Buffer_Error return Buffer_Error is
   begin
      return Buffer_Error'(
         Base => Make_Resource_Error(
            Kind => Not_Found,
            Resource_Type => "Ready Buffer",
            Message => "No ready buffers available",
            Recovery => "Wait for producer to mark buffer ready"
         ).Base
      );
   end Make_No_Ready_Buffer_Error;
   
   function Make_Invalid_Buffer_Index_Error (Index : Natural) return Buffer_Error is
   begin
      return Buffer_Error'(
         Base => Make_Validation_Error(
            Kind => Out_Of_Range,
            Field_Name => "Buffer Index",
            Invalid_Value => Index'Image,
            Message => "Invalid buffer index",
            Recovery => "Use index from 0 to " & Natural'Image (Buffer_Count_Value - 1)
         ).Base
      );
   end Make_Invalid_Buffer_Index_Error;

   --  Initialize buffer manager
   procedure Initialize 
     (Manager     : in out Buffer_Manager_Type;
      Buffer_Size : Natural := Default_Buffer_Size_Bytes) is
   begin
      if not Manager.Manager_Ready then
         Manager.Protected_Manager.Initialize_Buffers (Buffer_Size);
         Manager.Manager_Ready := True;
      end if;
   end Initialize;

   --  Check if initialized
   function Is_Initialized (Manager : Buffer_Manager_Type) return Boolean is
   begin
      return Manager.Manager_Ready and then Manager.Protected_Manager.Is_Active;
   end Is_Initialized;

   --  Check if shut down
   function Is_Shutdown (Manager : Buffer_Manager_Type) return Boolean is
   begin
      return not Manager.Protected_Manager.Is_Active;
   end Is_Shutdown;

   --  Get free buffer (non-blocking)
   function Get_Free_Buffer 
     (Manager : in out Buffer_Manager_Type) return Buffer_Result.Result is
      Buffer  : Stream_Element_Array_Access;
      Index   : Natural;
      Success : Boolean := False;
   begin
      Buffer := null;
      Index := Natural'Last;

      if not Manager.Is_Initialized then
         return Buffer_Result.Err (Make_Buffer_Not_Initialized_Error);
      end if;
      
      if Manager.Is_Shutdown then
         return Buffer_Result.Err (Make_Buffer_Shutdown_Error);
      end if;

      if Manager.Protected_Manager.Free_Available then
         select
            Manager.Protected_Manager.Get_Free_Buffer_Entry (Buffer, Index);
            Success := Buffer /= null;
         else
            Success := False;
         end select;
      else
         Success := False;
      end if;
      
      if Success then
         return Buffer_Result.Ok (
            Buffer_Info'(Buffer => Buffer,
                        Size   => 0,
                        Index  => Index));
      else
         return Buffer_Result.Err (Make_No_Free_Buffer_Error);
      end if;
   end Get_Free_Buffer;

   --  Mark buffer ready
   function Mark_Buffer_Ready 
     (Manager : in out Buffer_Manager_Type;
      Index   : Natural; 
      Size    : Natural) return Void_Result.Result is
   begin
      if Index >= Buffer_Count_Value then
         return Void_Result.Err (Make_Invalid_Buffer_Index_Error (Index));
      end if;
      
      Manager.Protected_Manager.Mark_Ready (Index, Size);
      return Void_Result.Ok (True);
   end Mark_Buffer_Ready;

   --  Mark buffer error
   function Mark_Buffer_Error 
     (Manager : in out Buffer_Manager_Type;
      Index   : Natural) return Void_Result.Result is
   begin
      if Index >= Buffer_Count_Value then
         return Void_Result.Err (Make_Invalid_Buffer_Index_Error (Index));
      end if;
      
      Manager.Protected_Manager.Mark_Error (Index);
      return Void_Result.Ok (True);
   end Mark_Buffer_Error;

   --  Try get ready buffer (non-blocking)
   function Try_Get_Ready_Buffer
     (Manager : in out Buffer_Manager_Type) return Buffer_Result.Result is
      Buffer  : Stream_Element_Array_Access;
      Size    : Natural;
      Index   : Natural;
      Success : Boolean := False;
   begin
      Buffer := null;
      Size := 0;
      Index := Natural'Last;

      if Manager.Protected_Manager.Ready_Available then
         select
            Manager.Protected_Manager.Get_Ready_Buffer_Entry (Buffer, Size, Index);
            Success := Buffer /= null;
         else
            Success := False;
         end select;
      else
         Success := False;
      end if;
      
      if Success then
         return Buffer_Result.Ok (
            Buffer_Info'(Buffer => Buffer,
                        Size   => Size,
                        Index  => Index));
      else
         return Buffer_Result.Err (Make_No_Ready_Buffer_Error);
      end if;
   end Try_Get_Ready_Buffer;

   --  Try transfer buffer ownership (non-blocking)
   function Try_Transfer_Buffer_Ownership
     (Manager : in out Buffer_Manager_Type) return Buffer_Result.Result is
      Buffer  : Stream_Element_Array_Access;
      Size    : Natural;
      Index   : Natural;
      Success : Boolean := False;
   begin
      Buffer := null;
      Size := 0;
      Index := Natural'Last;

      if Manager.Protected_Manager.Ready_Available then
         select
            Manager.Protected_Manager.Transfer_Ownership (Buffer, Size, Index);
            Success := Buffer /= null;
         else
            Success := False;
         end select;
      else
         Success := False;
      end if;
      
      if Success then
         return Buffer_Result.Ok (
            Buffer_Info'(Buffer => Buffer,
                        Size   => Size,
                        Index  => Index));
      else
         return Buffer_Result.Err (Make_No_Ready_Buffer_Error);
      end if;
   end Try_Transfer_Buffer_Ownership;

   --  Mark buffer consumed
   function Mark_Buffer_Consumed 
     (Manager : in out Buffer_Manager_Type;
      Index   : Natural) return Void_Result.Result is
   begin
      if Index >= Buffer_Count_Value then
         return Void_Result.Err (Make_Invalid_Buffer_Index_Error (Index));
      end if;
      Manager.Protected_Manager.Mark_Consumed (Index);
      return Void_Result.Ok (True);
   end Mark_Buffer_Consumed;

   --  Return buffer
   function Return_Buffer 
     (Manager : in out Buffer_Manager_Type;
      Buffer  : in out Stream_Element_Array_Access; 
      Index   : Natural) return Void_Result.Result is
   begin
      if Index >= Buffer_Count_Value then
         return Void_Result.Err (Make_Invalid_Buffer_Index_Error (Index));
      end if;
      
      Manager.Protected_Manager.Return_Transferred_Buffer (Buffer, Index);
      Buffer := null;
      return Void_Result.Ok (True);
   end Return_Buffer;

   --  State queries
   function Has_Ready_Buffer (Manager : Buffer_Manager_Type) return Boolean is
   begin
      return Manager.Protected_Manager.Ready_Available;
   end Has_Ready_Buffer;

   function Has_Free_Buffer (Manager : Buffer_Manager_Type) return Boolean is
   begin
      return Manager.Protected_Manager.Free_Available;
   end Has_Free_Buffer;

   function Buffer_Count_In_State 
     (Manager : Buffer_Manager_Type; 
      State   : Buffer_State_Type) return Natural is
   begin
      return Manager.Protected_Manager.Count_In_State (State);
   end Buffer_Count_In_State;

   --  Get statistics
   function Get_Statistics (Manager : Buffer_Manager_Type) return Buffer_Statistics_Type is
   begin
      return Manager.Protected_Manager.Get_Stats;
   end Get_Statistics;

   --  Get buffer size
   function Get_Buffer_Size (Manager : Buffer_Manager_Type) return Natural is
      Stats : constant Buffer_Statistics_Type := Manager.Get_Statistics;
   begin
      return Stats.Buffer_Size_Bytes;
   end Get_Buffer_Size;

   --  Shutdown
   procedure Shutdown (Manager : in out Buffer_Manager_Type) is
   begin
      Manager.Protected_Manager.Shutdown_Buffers;
      Manager.Manager_Ready := False;
   end Shutdown;

   --  RAII cleanup
   overriding
   procedure Finalize (Manager : in out Buffer_Manager_Type) is
   begin
      if Manager.Manager_Ready then
         Shutdown (Manager);
      end if;
   end Finalize;

   --  Protected manager implementation
   protected body Protected_Manager_Type is
   
      --  Initialize buffers
      procedure Initialize_Buffers (Buffer_Size : Natural) is
      begin
         if Is_Manager_Active then
            return;
         end if;
         
         Buffer_Size_Value := Buffer_Size;
         
         for I in Buffers'Range loop
            Buffers (I).Buffer := 
              new Stream_Element_Array (1 .. Stream_Element_Offset (Buffer_Size));
            Buffers (I).Size := 0;
            Buffers (I).State := Free;
         end loop;
         
         Is_Manager_Active := True;
      end Initialize_Buffers;
      
      --  Get free buffer entry
      entry Get_Free_Buffer_Entry 
        (Buffer : out Stream_Element_Array_Access;
         Index  : out Natural)
        when Is_Manager_Active and then
             (for some I in Buffers'Range => Buffers (I).State = Free) is
      begin
         Index := Find_Buffer_In_State (Free);
         if Index < Buffers'Length then
            Update_Buffer_State (Index, Reading);
            Buffer := Buffers (Index).Buffer;
         else
            Buffer := null;
         end if;
      end Get_Free_Buffer_Entry;
      
      --  Mark buffer ready
      procedure Mark_Ready (Index : Natural; Size : Natural) is
      begin
         if Index < Buffers'Length and then Buffers (Index).State = Reading then
            Buffers (Index).Size := Size;
            Update_Buffer_State (Index, Ready);
         end if;
      end Mark_Ready;
      
      --  Mark buffer error
      procedure Mark_Error (Index : Natural) is
      begin
         if Index < Buffers'Length then
            Update_Buffer_State (Index, Error);
         end if;
      end Mark_Error;
      
      --  Get ready buffer entry
      entry Get_Ready_Buffer_Entry
        (Buffer : out Stream_Element_Array_Access;
         Size   : out Natural;
         Index  : out Natural)
        when Is_Manager_Active and then
             (for some I in Buffers'Range => Buffers (I).State = Ready) is
      begin
         Index := Find_Buffer_In_State (Ready);
         if Index < Buffers'Length then
            Update_Buffer_State (Index, Consuming);
            Buffer := Buffers (Index).Buffer;
            Size := Buffers (Index).Size;
         else
            Buffer := null;
            Size := 0;
         end if;
      end Get_Ready_Buffer_Entry;
      
      --  Transfer ownership
      entry Transfer_Ownership
        (Buffer : out Stream_Element_Array_Access;
         Size   : out Natural;
         Index  : out Natural)
        when Is_Manager_Active and then
             (for some I in Buffers'Range => Buffers (I).State = Ready) is
      begin
         Index := Find_Buffer_In_State (Ready);
         if Index < Buffers'Length then
            --  Transfer ownership - consumer must manage memory
            Buffer := Buffers (Index).Buffer;
            Size := Buffers (Index).Size;
            
            --  Allocate new buffer for pool
            Buffers (Index).Buffer := 
              new Stream_Element_Array (1 .. Stream_Element_Offset (Buffer_Size_Value));
            Buffers (Index).Size := 0;
            Update_Buffer_State (Index, Free);
         else
            Buffer := null;
            Size := 0;
         end if;
      end Transfer_Ownership;
      
      --  Mark consumed
      procedure Mark_Consumed (Index : Natural) is
      begin
         if Index < Buffers'Length and then Buffers (Index).State = Consuming then
            Buffers (Index).Size := 0;
            Update_Buffer_State (Index, Free);
         end if;
      end Mark_Consumed;
      
      --  Return transferred buffer
      procedure Return_Transferred_Buffer 
        (Buffer : Stream_Element_Array_Access; 
         Index  : Natural) is
         Buffer_Copy : Stream_Element_Array_Access := Buffer;
      begin
         if Index < Buffers'Length and then Buffer /= null then
            --  Consumer returning transferred buffer
            if Buffers (Index).Buffer /= Buffer then
               Free_Stream_Element_Array (Buffer_Copy);
            end if;
         end if;
      end Return_Transferred_Buffer;
      
      --  State queries
      function Ready_Available return Boolean is
      begin
         return (for some I in Buffers'Range => Buffers (I).State = Ready);
      end Ready_Available;
      
      function Free_Available return Boolean is
      begin
         return (for some I in Buffers'Range => Buffers (I).State = Free);
      end Free_Available;
      
      function Count_In_State (State : Buffer_State_Type) return Natural is
         Count : Natural := 0;
      begin
         for I in Buffers'Range loop
            if Buffers (I).State = State then
               Count := Count + 1;
            end if;
         end loop;
         return Count;
      end Count_In_State;
      
      --  Get comprehensive statistics
      function Get_Stats return Buffer_Statistics_Type is
         Stats : Buffer_Statistics_Type;
      begin
         Stats.Total_Buffers_Count := Buffers'Length;
         Stats.Free_Buffers_Count := Count_In_State (Free);
         Stats.Reading_Buffers_Count := Count_In_State (Reading);
         Stats.Ready_Buffers_Count := Count_In_State (Ready);
         Stats.Consuming_Buffers_Count := Count_In_State (Consuming);
         Stats.Error_Buffers_Count := Count_In_State (Error);
         Stats.Buffer_Size_Bytes := Buffer_Size_Value;
         return Stats;
      end Get_Stats;
      
      --  Shutdown
      procedure Shutdown_Buffers is
      begin
         Is_Manager_Active := False;
         
         for I in Buffers'Range loop
            Free_Buffer_Memory (I);
         end loop;
      end Shutdown_Buffers;
      
      function Is_Active return Boolean is
      begin
         return Is_Manager_Active;
      end Is_Active;
      
      --  Helper: Find buffer in state
      function Find_Buffer_In_State (State : Buffer_State_Type) return Natural is
      begin
         for I in Buffers'Range loop
            if Buffers (I).State = State then
               return I;
            end if;
         end loop;
         return Buffers'Length;  -- Not found sentinel
      end Find_Buffer_In_State;
      
      --  Helper: Free buffer memory
      procedure Free_Buffer_Memory (Index : Natural) is
      begin
         if Index < Buffers'Length and then Buffers (Index).Buffer /= null then
            Free_Stream_Element_Array (Buffers (Index).Buffer);
            Buffers (Index).State := Free;
            Buffers (Index).Size := 0;
         end if;
      end Free_Buffer_Memory;
      
      --  Helper: Update buffer state
      procedure Update_Buffer_State (Index : Natural; State : Buffer_State_Type) is
      begin
         if Index < Buffers'Length then
            Buffers (Index).State := State;
         end if;
      end Update_Buffer_State;
      
   end Protected_Manager_Type;

end Abohlib.Core.Domain.Utilities.Concurrent.Buffer_Manager;