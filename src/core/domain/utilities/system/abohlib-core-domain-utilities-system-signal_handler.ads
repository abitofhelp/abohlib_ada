--  =============================================================================
--  Abohlib.Core.Domain.Utilities.System.Signal_Handler - Signal Handler Interface
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Cross-platform signal handling interface. Provides abstraction for
--  graceful shutdown on SIGINT, SIGTERM, and other termination signals.
--  Designed to work with the cancellation source pattern for clean shutdown.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Ports.Concurrent.Cancellation;
use Abohlib.Core.Domain.Ports.Concurrent.Cancellation;

package Abohlib.Core.Domain.Utilities.System.Signal_Handler is

   pragma Preelaborate;

   --  Supported signal types
   type Signal_Type is 
     (SIGINT,      -- Interactive interrupt (Ctrl+C)
      SIGTERM,     -- Termination request
      SIGHUP,      -- Hangup
      SIGQUIT,     -- Quit
      SIGUSR1,     -- User-defined signal 1
      SIGUSR2,     -- User-defined signal 2
      Unknown);    -- Unknown/unsupported signal

   --  Convert signal type to string
   function To_String (Signal : Signal_Type) return String is
     (case Signal is
        when SIGINT  => "SIGINT",
        when SIGTERM => "SIGTERM",
        when SIGHUP  => "SIGHUP",
        when SIGQUIT => "SIGQUIT",
        when SIGUSR1 => "SIGUSR1",
        when SIGUSR2 => "SIGUSR2",
        when Unknown => "UNKNOWN");

   --  Signal handler interface
   type Signal_Handler_Interface is interface;
   type Signal_Handler_Access is access all Signal_Handler_Interface'Class;

   --  Install signal handlers for graceful shutdown
   procedure Install_Handlers
     (Handler      : in out Signal_Handler_Interface;
      Cancellation : Cancellation_Source_Access;
      Signals      : Signal_Type := SIGINT)  -- Default to SIGINT only
   is abstract
   with
     Pre'Class => Cancellation /= null and then not Handler.Is_Installed,
     Post'Class => Handler.Is_Installed;

   --  Array type for signal lists
   type Signal_Array_Type is array (Positive range <>) of Signal_Type;

   --  Install handlers for multiple signals
   procedure Install_Multiple_Handlers
     (Handler      : in out Signal_Handler_Interface;
      Cancellation : Cancellation_Source_Access;
      Signals      : Signal_Array_Type)
   is abstract
   with
     Pre'Class => Cancellation /= null and then not Handler.Is_Installed,
     Post'Class => Handler.Is_Installed;

   --  Remove signal handlers (restore defaults)
   procedure Remove_Handlers (Handler : in out Signal_Handler_Interface)
   is abstract
   with
     Pre'Class => Handler.Is_Installed,
     Post'Class => not Handler.Is_Installed;

   --  Check if handler is installed
   function Is_Installed (Handler : Signal_Handler_Interface) return Boolean
   is abstract;

   --  Get last signal received
   function Last_Signal (Handler : Signal_Handler_Interface) return Signal_Type
   is abstract
   with Post'Class => (if not Handler.Is_Installed then 
                        Last_Signal'Result = Unknown);

   --  Get signal count (total signals received)
   function Signal_Count (Handler : Signal_Handler_Interface) return Natural
   is abstract
   with Post'Class => (if not Handler.Is_Installed then 
                        Signal_Count'Result = 0);

   --  Platform-specific signal handler creation
   type Platform_Type is (Linux, MacOS, Windows, FreeBSD, Unknown);

   --  Detect current platform
   function Current_Platform return Platform_Type;

   --  Factory function to create platform-specific handler
   function Create_Platform_Handler return Signal_Handler_Interface'Class;

end Abohlib.Core.Domain.Utilities.System.Signal_Handler;