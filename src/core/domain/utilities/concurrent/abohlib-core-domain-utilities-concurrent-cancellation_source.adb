--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Concurrent.Cancellation_Source - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Utilities.Concurrent.Cancellation_Source is

   --  Protected state implementation
   protected body Protected_Cancellation_State is
      
      function Is_Cancelled return Boolean is
      begin
         return Cancelled;
      end Is_Cancelled;
      
      procedure Request_Cancellation (Reason : String) is
         Copy_Length : constant Natural := 
           Natural'Min (Reason'Length, MAX_REASON_LENGTH);
      begin
         Cancelled := True;
         if Copy_Length > 0 then
            Reason_Text (1 .. Copy_Length) := Reason (Reason'First .. Reason'First + Copy_Length - 1);
            Reason_Length := Copy_Length;
         else
            Reason_Text (1 .. 22) := "Cancellation requested";
            Reason_Length := 22;
         end if;
      end Request_Cancellation;
      
      procedure Reset is
      begin
         Cancelled := False;
         Reason_Length := 0;
         Reason_Text := [others => ' '];
      end Reset;
      
      function Get_Reason return String is
      begin
         if Reason_Length > 0 then
            return Reason_Text (1 .. Reason_Length);
         else
            return "";
         end if;
      end Get_Reason;
      
   end Protected_Cancellation_State;

   --  Create a new cancellation source
   function Create return Cancellation_Source_Type is
   begin
      return (Ada.Finalization.Limited_Controlled with State => <>);
   end Create;

   --  Basic cancellation source implementation
   overriding
   function Is_Cancelled
     (Source : Cancellation_Source_Type) return Boolean is
   begin
      return Source.State.Is_Cancelled;
   end Is_Cancelled;

   overriding
   procedure Request_Cancellation
     (Source : in out Cancellation_Source_Type;
      Reason : String := "") is
   begin
      Source.State.Request_Cancellation (Reason);
   end Request_Cancellation;

   overriding
   procedure Reset (Source : in out Cancellation_Source_Type) is
   begin
      Source.State.Reset;
   end Reset;

   overriding
   function Cancellation_Reason
     (Source : Cancellation_Source_Type) return String is
   begin
      return Source.State.Get_Reason;
   end Cancellation_Reason;

   overriding
   function Create_Linked_Token
     (Source : Cancellation_Source_Type)
      return Cancellation_Source_Interface'Class is
      pragma Unreferenced (Source);
   begin
      raise Program_Error with 
        "Use Create_Linked_Token_From_Access for proper parent linkage";
      return Create;  -- Never reached
   end Create_Linked_Token;

   overriding
   function Is_Linked
     (Source : Cancellation_Source_Type) return Boolean is
   begin
      return False;  -- Basic source is never linked
   end Is_Linked;

   overriding
   function Parent_Source
     (Source : Cancellation_Source_Type) 
      return Cancellation_Source_Access is
      pragma Unreferenced (Source);
   begin
      raise Program_Error with "Basic cancellation source has no parent";
      return null;
   end Parent_Source;

   overriding
   procedure Finalize (Source : in out Cancellation_Source_Type) is
   begin
      Source.State.Reset;
   end Finalize;

   --  Linked cancellation token implementation
   overriding
   function Is_Cancelled
     (Source : Linked_Cancellation_Token_Type) return Boolean is
   begin
      --  Cancelled if either own state or parent is cancelled
      return Source.Own_State.Is_Cancelled or else
             (Source.Parent /= null and then Source.Parent.Is_Cancelled);
   end Is_Cancelled;

   overriding
   procedure Request_Cancellation
     (Source : in out Linked_Cancellation_Token_Type;
      Reason : String := "") is
   begin
      Source.Own_State.Request_Cancellation (Reason);
   end Request_Cancellation;

   overriding
   procedure Reset (Source : in out Linked_Cancellation_Token_Type) is
   begin
      Source.Own_State.Reset;
      --  Note: Does not reset parent
   end Reset;

   overriding
   function Cancellation_Reason
     (Source : Linked_Cancellation_Token_Type) return String is
   begin
      --  Return own reason if cancelled locally
      if Source.Own_State.Is_Cancelled then
         return Source.Own_State.Get_Reason;
      --  Otherwise return parent reason if parent is cancelled
      elsif Source.Parent /= null and then Source.Parent.Is_Cancelled then
         return Source.Parent.Cancellation_Reason;
      else
         return "";
      end if;
   end Cancellation_Reason;

   overriding
   function Create_Linked_Token
     (Source : Linked_Cancellation_Token_Type)
      return Cancellation_Source_Interface'Class is
   begin
      --  Create another linked token with same parent
      if Source.Parent /= null then
         return Create_Linked_Token_From_Access (Source.Parent);
      else
         raise Program_Error with "Linked token has no parent";
      end if;
   end Create_Linked_Token;

   overriding
   function Is_Linked
     (Source : Linked_Cancellation_Token_Type) return Boolean is
      pragma Unreferenced (Source);
   begin
      return True;
   end Is_Linked;

   overriding
   function Parent_Source
     (Source : Linked_Cancellation_Token_Type) 
      return Cancellation_Source_Access is
   begin
      return Source.Parent;
   end Parent_Source;

   overriding
   procedure Finalize (Source : in out Linked_Cancellation_Token_Type) is
   begin
      Source.Own_State.Reset;
      Source.Parent := null;
   end Finalize;
   
   --  Create a linked token with proper parent reference
   function Create_Linked_Token_From_Access
     (Parent : not null Cancellation_Source_Access)
      return Cancellation_Source_Interface'Class is
   begin
      return Token : Linked_Cancellation_Token_Type do
         Token.Parent := Parent;
      end return;
   end Create_Linked_Token_From_Access;

end Abohlib.Core.Domain.Utilities.Concurrent.Cancellation_Source;