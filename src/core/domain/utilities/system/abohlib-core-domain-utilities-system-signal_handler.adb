--  =============================================================================
--  Abohlib.Core.Domain.Utilities.System.Signal_Handler - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with System;

package body Abohlib.Core.Domain.Utilities.System.Signal_Handler is

   package Ada_System renames Standard.System;

   --  Platform detection (simplified to avoid warnings)
   function Current_Platform return Platform_Type is
      pragma Warnings (Off, "condition is always*");
      System_Name : constant String := Ada_System.System_Name'Image;
   begin
      if System_Name = "LINUX" or else System_Name = "SYSTEM_NAME_GNAT" then
         return Linux;
      elsif System_Name = "DARWIN" or else System_Name = "MACOSX" then
         return MacOS;
      elsif System_Name = "WINDOWS" or else System_Name = "MINGW32" then
         return Windows;
      elsif System_Name = "FREEBSD" then
         return FreeBSD;
      else
         --  Default to unknown if system name is not recognized
         return Unknown;
      end if;
      pragma Warnings (On, "condition is always*");
   end Current_Platform;

   --  Generic fallback handler for unsupported platforms
   type Generic_Signal_Handler_Type is new Signal_Handler_Interface with record
      Installed : Boolean := False;
      Last : Signal_Type := Unknown;
      Count : Natural := 0;
   end record;

   --  Overriding procedure declarations
   overriding
   procedure Install_Handlers
     (Handler      : in out Generic_Signal_Handler_Type;
      Cancellation : Cancellation_Source_Access;
      Signals      : Signal_Type := SIGINT);

   overriding
   procedure Install_Multiple_Handlers
     (Handler      : in out Generic_Signal_Handler_Type;
      Cancellation : Cancellation_Source_Access;
      Signals      : Signal_Array_Type);

   overriding
   procedure Remove_Handlers (Handler : in out Generic_Signal_Handler_Type);

   overriding
   function Is_Installed (Handler : Generic_Signal_Handler_Type) return Boolean;

   overriding
   function Last_Signal (Handler : Generic_Signal_Handler_Type) return Signal_Type;

   overriding
   function Signal_Count (Handler : Generic_Signal_Handler_Type) return Natural;

   --  Overriding procedure implementations
   overriding
   procedure Install_Handlers
     (Handler      : in out Generic_Signal_Handler_Type;
      Cancellation : Cancellation_Source_Access;
      Signals      : Signal_Type := SIGINT) is
      pragma Unreferenced (Cancellation, Signals);
   begin
      Handler.Installed := True;
      --  Generic implementation does nothing
   end Install_Handlers;

   overriding
   procedure Install_Multiple_Handlers
     (Handler      : in out Generic_Signal_Handler_Type;
      Cancellation : Cancellation_Source_Access;
      Signals      : Signal_Array_Type) is
      pragma Unreferenced (Cancellation, Signals);
   begin
      Handler.Installed := True;
      --  Generic implementation does nothing
   end Install_Multiple_Handlers;

   overriding
   procedure Remove_Handlers (Handler : in out Generic_Signal_Handler_Type) is
   begin
      Handler.Installed := False;
      Handler.Last := Unknown;
      Handler.Count := 0;
   end Remove_Handlers;

   overriding
   function Is_Installed (Handler : Generic_Signal_Handler_Type) return Boolean is
   begin
      return Handler.Installed;
   end Is_Installed;

   overriding
   function Last_Signal (Handler : Generic_Signal_Handler_Type) return Signal_Type is
   begin
      return Handler.Last;
   end Last_Signal;

   overriding
   function Signal_Count (Handler : Generic_Signal_Handler_Type) return Natural is
   begin
      return Handler.Count;
   end Signal_Count;

   --  Factory function creates platform-specific handler
   function Create_Platform_Handler return Signal_Handler_Interface'Class is
   begin
      case Current_Platform is
         when Linux | MacOS | FreeBSD =>
            --  Would create POSIX signal handler here
            --  For now, return generic handler
            return Generic_Signal_Handler_Type'(others => <>);
            
         when Windows =>
            --  Would create Windows console handler here
            --  For now, return generic handler
            return Generic_Signal_Handler_Type'(others => <>);
            
         when Unknown =>
            --  Return generic handler for unknown platforms
            return Generic_Signal_Handler_Type'(others => <>);
      end case;
   end Create_Platform_Handler;

end Abohlib.Core.Domain.Utilities.System.Signal_Handler;