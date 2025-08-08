--  =============================================================================
--  Abohlib.Core.Domain.Events - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Numerics.Discrete_Random;

package body Abohlib.Core.Domain.Events is

   --  Simple ULID generation (placeholder - in production use proper ULID library)
   subtype ULID_Char is Character range '0' .. 'Z';
   package Random_Char is new Ada.Numerics.Discrete_Random (ULID_Char);
   Gen : Random_Char.Generator;

   function Generate_ULID return ULID_String is
      Result : ULID_String;
   begin
      --  This is a simplified ULID generation
      --  Real ULID should encode timestamp and randomness
      for I in Result'Range loop
         Result (I) := Random_Char.Random (Gen);
      end loop;
      return Result;
   end Generate_ULID;

   --  ==========================================================================
   --  Event Initialization
   --  ==========================================================================

   procedure Initialize_Event (Event : in out Domain_Event'Class) is
   begin
      Event.Event_Id := Generate_ULID;
      Event.Occurred_At := Ada.Calendar.Clock;
   end Initialize_Event;

begin
   --  Initialize random generator
   Random_Char.Reset (Gen);
end Abohlib.Core.Domain.Events;
