--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Concurrent.Lock_Free_Queue_Adapter - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Utilities.Concurrent.Lock_Free_Queue_Adapter is

   package body Queue_Adapter is

      overriding
      procedure Initialize (Queue : in out Lock_Free_Queue_Type) is
      begin
         Ring_Buffer.Initialize (Queue.Buffer);
      end Initialize;

      overriding
      function Try_Push
        (Queue : in out Lock_Free_Queue_Type; Item : Element_Type)
         return Boolean is
         Result : constant Ring_Buffer.Push_Result.Result := 
            Ring_Buffer.Try_Push (Queue.Buffer, Item);
      begin
         return Ring_Buffer.Push_Result.Is_Ok (Result);
      end Try_Push;

      overriding
      function Try_Pop
        (Queue : in out Lock_Free_Queue_Type; Item : out Element_Type)
         return Boolean is
         Result : constant Ring_Buffer.Pop_Result.Result := 
            Ring_Buffer.Try_Pop (Queue.Buffer);
      begin
         if Ring_Buffer.Pop_Result.Is_Ok (Result) then
            Item := Ring_Buffer.Pop_Result.Get_Ok (Result);
            return True;
         else
            return False;
         end if;
      end Try_Pop;

      overriding
      function Is_Empty (Queue : Lock_Free_Queue_Type) return Boolean is
      begin
         return Ring_Buffer.Is_Empty (Queue.Buffer);
      end Is_Empty;

      overriding
      function Is_Full (Queue : Lock_Free_Queue_Type) return Boolean is
      begin
         return Ring_Buffer.Is_Full (Queue.Buffer);
      end Is_Full;

      overriding
      function Size (Queue : Lock_Free_Queue_Type) return Natural is
      begin
         return Ring_Buffer.Size (Queue.Buffer);
      end Size;

   end Queue_Adapter;

end Abohlib.Core.Domain.Utilities.Concurrent.Lock_Free_Queue_Adapter;