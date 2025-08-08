--  =============================================================================
--  Abohlib.Core.Domain.Ports.Concurrent.Cancellation - Cancellation Port Interface
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Interface for cancellation/interruption handling. Allows graceful shutdown
--  of processing operations in response to signals or user requests.
--  Supports hierarchical cancellation through linked tokens.
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Ports.Concurrent.Cancellation is

   pragma Preelaborate;

   --  Cancellation source interface
   type Cancellation_Source_Interface is limited interface;
   type Cancellation_Source_Access is
     access all Cancellation_Source_Interface'Class;

   --  Check if cancellation has been requested
   function Is_Cancelled
     (Source : Cancellation_Source_Interface) return Boolean
   is abstract
   with Post'Class => Is_Cancelled'Result = (Source.Cancellation_Reason'Length > 0);

   --  Request cancellation with optional reason
   procedure Request_Cancellation
     (Source : in out Cancellation_Source_Interface;
      Reason : String := "")
   is abstract
   with Post'Class => Source.Is_Cancelled;

   --  Reset cancellation state
   procedure Reset (Source : in out Cancellation_Source_Interface) is abstract
   with Post'Class => not Source.Is_Cancelled;

   --  Get cancellation reason (empty string if not cancelled)
   function Cancellation_Reason
     (Source : Cancellation_Source_Interface) return String
   is abstract
   with Post'Class => (if not Source.Is_Cancelled then 
                        Cancellation_Reason'Result'Length = 0);

   --  Create a linked token for hierarchical cancellation
   --  When parent is cancelled, all linked tokens are also cancelled
   function Create_Linked_Token
     (Source : Cancellation_Source_Interface)
      return Cancellation_Source_Interface'Class
   is abstract
   with Post'Class => (if Source.Is_Cancelled then 
                        Create_Linked_Token'Result.Is_Cancelled);

   --  Check if this is a linked token
   function Is_Linked
     (Source : Cancellation_Source_Interface) return Boolean
   is abstract;

   --  Get parent source if this is a linked token
   function Parent_Source
     (Source : Cancellation_Source_Interface) 
      return Cancellation_Source_Access
   is abstract
   with Pre'Class => Source.Is_Linked;

end Abohlib.Core.Domain.Ports.Concurrent.Cancellation;