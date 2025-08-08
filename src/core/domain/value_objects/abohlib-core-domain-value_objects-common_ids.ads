--  =============================================================================
--  Abohlib.Core.Domain.Value_Objects.Common_IDs - Common ID Type Instantiations
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides pre-instantiated ID types for common use cases to avoid generic
--    instantiation ordering issues. These are commonly used ID types that can
--    be reused across applications.
--
--  Usage:
--    Instead of instantiating Generic_ID_Type directly in your code, use these
--    pre-defined packages:
--    - Entity_ID for domain entities
--    - Event_ID for domain events
--    - Request_ID for API/command requests
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id;
use Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id;

package Abohlib.Core.Domain.Value_Objects.Common_IDs is

   --  Example categories for common use cases
   type Entity_Category is null record;
   type Aggregate_Category is null record;
   type Event_Category is null record;
   type Command_Category is null record;
   type Request_Category is null record;

   --  Pre-defined ID packages for common cases
   package Entity_ID is new
     Generic_ID_Type
       (Category      => Entity_Category,
        Category_Name => "Entity",
        Prefix        => "ent_");

   package Event_ID is new
     Generic_ID_Type
       (Category      => Event_Category,
        Category_Name => "Event",
        Prefix        => "evt_");

   package Request_ID is new
     Generic_ID_Type
       (Category      => Request_Category,
        Category_Name => "Request",
        Prefix        => "req_");

end Abohlib.Core.Domain.Value_Objects.Common_IDs;
