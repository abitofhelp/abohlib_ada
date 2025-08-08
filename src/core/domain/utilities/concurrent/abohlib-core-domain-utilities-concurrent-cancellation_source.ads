--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Concurrent.Cancellation_Source
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Concrete implementation of the cancellation source interface.
--  Provides thread-safe cancellation with support for hierarchical tokens.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Ports.Concurrent.Cancellation;
use Abohlib.Core.Domain.Ports.Concurrent.Cancellation;
with Ada.Finalization;

package Abohlib.Core.Domain.Utilities.Concurrent.Cancellation_Source is

   pragma Preelaborate;

   --  Maximum cancellation reason length
   MAX_REASON_LENGTH : constant := 256;

   --  Basic cancellation source implementation
   type Cancellation_Source_Type is limited new 
     Ada.Finalization.Limited_Controlled and
     Cancellation_Source_Interface with private;

   --  Create a new cancellation source
   function Create return Cancellation_Source_Type;
   
   --  Create a linked token with proper parent reference
   --  This is a helper to work around the interface limitation
   function Create_Linked_Token_From_Access
     (Parent : not null Cancellation_Source_Access)
      return Cancellation_Source_Interface'Class;

   --  Implement Cancellation_Source_Interface
   overriding
   function Is_Cancelled
     (Source : Cancellation_Source_Type) return Boolean;

   overriding
   procedure Request_Cancellation
     (Source : in out Cancellation_Source_Type;
      Reason : String := "");

   overriding
   procedure Reset (Source : in out Cancellation_Source_Type);

   overriding
   function Cancellation_Reason
     (Source : Cancellation_Source_Type) return String;

   overriding
   function Create_Linked_Token
     (Source : Cancellation_Source_Type)
      return Cancellation_Source_Interface'Class;

   overriding
   function Is_Linked
     (Source : Cancellation_Source_Type) return Boolean;

   overriding
   function Parent_Source
     (Source : Cancellation_Source_Type) 
      return Cancellation_Source_Access;

   --  RAII cleanup
   overriding
   procedure Finalize (Source : in out Cancellation_Source_Type);

   --  Linked cancellation token implementation
   type Linked_Cancellation_Token_Type is limited new
     Ada.Finalization.Limited_Controlled and
     Cancellation_Source_Interface with private;

private

   --  Protected type for thread-safe state management
   protected type Protected_Cancellation_State is
      
      function Is_Cancelled return Boolean;
      
      procedure Request_Cancellation (Reason : String);
      
      procedure Reset;
      
      function Get_Reason return String;
      
   private
      Cancelled : Boolean := False;
      Reason_Text : String (1 .. MAX_REASON_LENGTH) := [others => ' '];
      Reason_Length : Natural := 0;
   end Protected_Cancellation_State;

   --  Basic cancellation source
   type Cancellation_Source_Type is limited new 
     Ada.Finalization.Limited_Controlled and
     Cancellation_Source_Interface with record
      State : Protected_Cancellation_State;
   end record;

   --  Linked token that monitors parent
   type Linked_Cancellation_Token_Type is limited new
     Ada.Finalization.Limited_Controlled and
     Cancellation_Source_Interface with record
      Parent : Cancellation_Source_Access;
      Own_State : Protected_Cancellation_State;
   end record;

   --  Implement linked token interface
   overriding
   function Is_Cancelled
     (Source : Linked_Cancellation_Token_Type) return Boolean;

   overriding
   procedure Request_Cancellation
     (Source : in out Linked_Cancellation_Token_Type;
      Reason : String := "");

   overriding
   procedure Reset (Source : in out Linked_Cancellation_Token_Type);

   overriding
   function Cancellation_Reason
     (Source : Linked_Cancellation_Token_Type) return String;

   overriding
   function Create_Linked_Token
     (Source : Linked_Cancellation_Token_Type)
      return Cancellation_Source_Interface'Class;

   overriding
   function Is_Linked
     (Source : Linked_Cancellation_Token_Type) return Boolean;

   overriding
   function Parent_Source
     (Source : Linked_Cancellation_Token_Type) 
      return Cancellation_Source_Access;

   overriding
   procedure Finalize (Source : in out Linked_Cancellation_Token_Type);

end Abohlib.Core.Domain.Utilities.Concurrent.Cancellation_Source;