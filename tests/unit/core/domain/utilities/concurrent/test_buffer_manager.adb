--   =============================================================================
--   Test_Buffer_Manager - Unit tests for Concurrent Buffer Manager
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams; use Ada.Streams;
with Abohlib.Core.Domain.Utilities.Concurrent.Buffer_Manager;

package body Test_Buffer_Manager is

--   Instantiate buffer manager for testing
   pragma Warnings (Off, "potentially unsynchronized barrier");
   pragma Warnings (Off, "should be private component");
   package Test_BM is new
     Abohlib.Core.Domain.Utilities.Concurrent.Buffer_Manager
       (Buffer_Count_Value       => 4,
        Default_Buffer_Size_Bytes => 1024);
   pragma Warnings (On, "potentially unsynchronized barrier");
   pragma Warnings (On, "should be private component");
   use Test_BM;

--   Avoid naming conflicts by renaming Void_Result packages
   package BM_Void_Result renames Test_BM.Void_Result;
   package BM_Buffer_Result renames Test_BM.Buffer_Result;

--   Test buffer manager initialization
   function Test_Initialization return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Result is
      Manager : Buffer_Manager_Type;
      Stats   : Buffer_Statistics_Type;
   begin
--   Test uninitialized state
      if Is_Initialized (Manager) then
         return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Manager should not be initialized initially"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Initialization")
         ));
      end if;

      if not Is_Shutdown (Manager) then
         return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Manager should be shut down initially"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Initialization")
         ));
      end if;

--   Initialize manager
      Initialize (Manager, 2048);

--   Test initialized state
      if not Is_Initialized (Manager) then
         Shutdown (Manager);
         return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Manager should be initialized after Initialize"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Initialization")
         ));
      end if;

      if Is_Shutdown (Manager) then
         Shutdown (Manager);
         return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Manager should not be shut down after Initialize"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Initialization")
         ));
      end if;

--   Check statistics
      Stats := Get_Statistics (Manager);
      if Stats.Total_Buffers_Count /= 4 then
         Shutdown (Manager);
         return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Total buffers should be 4"),
            Details     => To_Unbounded_String ("Got: " & Stats.Total_Buffers_Count'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Initialization")
         ));
      end if;

      if Stats.Free_Buffers_Count /= 4 then
         Shutdown (Manager);
         return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Free buffers should be 4"),
            Details     => To_Unbounded_String ("Got: " & Stats.Free_Buffers_Count'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Initialization")
         ));
      end if;

      if Stats.Buffer_Size_Bytes /= 2048 then
         Shutdown (Manager);
         return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Buffer size should be 2048"),
            Details     => To_Unbounded_String ("Got: " & Stats.Buffer_Size_Bytes'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Initialization")
         ));
      end if;

      if Get_Buffer_Size (Manager) /= 2048 then
         Shutdown (Manager);
         return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Get_Buffer_Size should return 2048"),
            Details     => To_Unbounded_String ("Got: " & Get_Buffer_Size (Manager)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Initialization")
         ));
      end if;

--   Test availability queries
      if not Has_Free_Buffer (Manager) then
         Shutdown (Manager);
         return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Should have free buffers initially"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Initialization")
         ));
      end if;

      if Has_Ready_Buffer (Manager) then
         Shutdown (Manager);
         return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Should not have ready buffers initially"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Initialization")
         ));
      end if;

      Shutdown (Manager);
      return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Ok (True);
   end Test_Initialization;

--   Test producer operations
   function Test_Producer_Operations return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Result is
      Manager : Buffer_Manager_Type;
   begin
      Initialize (Manager, 1024);

--   Get free buffer
      declare
         Result : constant BM_Buffer_Result.Result := Get_Free_Buffer (Manager);
      begin
         if not BM_Buffer_Result.Is_Ok (Result) then
            Shutdown (Manager);
            return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Should get free buffer successfully"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Producer_Operations")
            ));
         end if;

         declare
            Info : constant Buffer_Info := BM_Buffer_Result.Get_Ok (Result);
         begin
            if Info.Buffer = null then
               Shutdown (Manager);
               return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Buffer should not be null"),
                  Details     => Null_Unbounded_String,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Producer_Operations")
               ));
            end if;

            if Buffer_Count_In_State (Manager, Reading) /= 1 then
               Shutdown (Manager);
               return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Should have 1 reading buffer"),
                  Details     => To_Unbounded_String ("Got: " & Buffer_Count_In_State (Manager, Reading)'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Producer_Operations")
               ));
            end if;

--   Fill buffer with test data
            declare
               Test_Data : constant String := "Hello, Buffer Manager!";
               Data_Size : constant Natural := Test_Data'Length;
            begin
               for I in Test_Data'Range loop
                  Info.Buffer (Stream_Element_Offset (I - Test_Data'First + 1)) :=
                    Stream_Element (Character'Pos (Test_Data (I)));
               end loop;

--   Mark buffer ready
               declare
                  Mark_Result : constant BM_Void_Result.Result :=
                    Mark_Buffer_Ready (Manager, Info.Index, Data_Size);
               begin
                  if not BM_Void_Result.Is_Ok (Mark_Result) then
                     Shutdown (Manager);
                     return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                        Kind        => Assertion_Failed,
                        Message     => To_Unbounded_String ("Should mark buffer ready successfully"),
                        Details     => Null_Unbounded_String,
                        Line_Number => 0,
                        Test_Name   => To_Unbounded_String ("Test_Producer_Operations")
                     ));
                  end if;
               end;

               if Buffer_Count_In_State (Manager, Ready) /= 1 then
                  Shutdown (Manager);
                  return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Should have 1 ready buffer"),
                     Details     => To_Unbounded_String ("Got: " & Buffer_Count_In_State (Manager, Ready)'Image),
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Producer_Operations")
                  ));
               end if;

               if not Has_Ready_Buffer (Manager) then
                  Shutdown (Manager);
                  return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Should have ready buffer"),
                     Details     => Null_Unbounded_String,
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Producer_Operations")
                  ));
               end if;
            end;
         end;
      end;

      Shutdown (Manager);
      return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Ok (True);
   end Test_Producer_Operations;

--   Test consumer operations
   function Test_Consumer_Operations return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Result is
      Manager     : Buffer_Manager_Type;
      Test_Data   : constant String := "Consumer Test Data";
   begin
      Initialize (Manager, 1024);

--   Producer: Get buffer and fill it
      declare
         Get_Result : constant BM_Buffer_Result.Result := Get_Free_Buffer (Manager);
      begin
         if not BM_Buffer_Result.Is_Ok (Get_Result) then
            Shutdown (Manager);
            return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Producer should get buffer"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Consumer_Operations")
            ));
         end if;

         declare
            Producer_Info : constant Buffer_Info := BM_Buffer_Result.Get_Ok (Get_Result);
         begin
--   Fill with test data
            for I in Test_Data'Range loop
               Producer_Info.Buffer (Stream_Element_Offset (I - Test_Data'First + 1)) :=
                 Stream_Element (Character'Pos (Test_Data (I)));
            end loop;

            declare
               Mark_Result : constant BM_Void_Result.Result :=
                 Mark_Buffer_Ready (Manager, Producer_Info.Index, Test_Data'Length);
            begin
               if not BM_Void_Result.Is_Ok (Mark_Result) then
                  Shutdown (Manager);
                  return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Should mark buffer ready"),
                     Details     => Null_Unbounded_String,
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Consumer_Operations")
                  ));
               end if;
            end;

--   Consumer: Get ready buffer
            declare
               Consumer_Result : constant BM_Buffer_Result.Result := Try_Get_Ready_Buffer (Manager);
            begin
               if not BM_Buffer_Result.Is_Ok (Consumer_Result) then
                  Shutdown (Manager);
                  return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Consumer should get ready buffer"),
                     Details     => Null_Unbounded_String,
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Consumer_Operations")
                  ));
               end if;

               declare
                  Consumer_Info : constant Buffer_Info := BM_Buffer_Result.Get_Ok (Consumer_Result);
               begin
                  if Consumer_Info.Buffer = null then
                     Shutdown (Manager);
                     return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                        Kind        => Assertion_Failed,
                        Message     => To_Unbounded_String ("Consumer buffer should not be null"),
                        Details     => Null_Unbounded_String,
                        Line_Number => 0,
                        Test_Name   => To_Unbounded_String ("Test_Consumer_Operations")
                     ));
                  end if;

                  if Consumer_Info.Size /= Test_Data'Length then
                     Shutdown (Manager);
                     return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                        Kind        => Assertion_Failed,
                        Message     => To_Unbounded_String ("Data size should match"),
                        Details     => To_Unbounded_String ("Expected: " & Test_Data'Length'Image &
                                      ", Got: " & Consumer_Info.Size'Image),
                        Line_Number => 0,
                        Test_Name   => To_Unbounded_String ("Test_Consumer_Operations")
                     ));
                  end if;

                  if Buffer_Count_In_State (Manager, Consuming) /= 1 then
                     Shutdown (Manager);
                     return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                        Kind        => Assertion_Failed,
                        Message     => To_Unbounded_String ("Should have 1 consuming buffer"),
                        Details     => To_Unbounded_String ("Got: " & Buffer_Count_In_State (Manager, Consuming)'Image),
                        Line_Number => 0,
                        Test_Name   => To_Unbounded_String ("Test_Consumer_Operations")
                     ));
                  end if;

--   Verify data integrity
                  declare
                     Retrieved_Data : String (1 .. Consumer_Info.Size);
                  begin
                     for I in 1 .. Consumer_Info.Size loop
                        Retrieved_Data (I) := Character'Val (Consumer_Info.Buffer (Stream_Element_Offset (I)));
                     end loop;

                     if Retrieved_Data /= Test_Data then
                        Shutdown (Manager);
                        return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                           Kind        => Assertion_Failed,
                           Message     => To_Unbounded_String ("Data should match original"),
                           Details     => To_Unbounded_String ("Expected: '" & Test_Data &
                                         "', Got: '" & Retrieved_Data & "'"),
                           Line_Number => 0,
                           Test_Name   => To_Unbounded_String ("Test_Consumer_Operations")
                        ));
                     end if;
                  end;

--   Mark consumed
                  declare
                     Consumed_Result : constant BM_Void_Result.Result :=
                       Mark_Buffer_Consumed (Manager, Consumer_Info.Index);
                  begin
                     if not BM_Void_Result.Is_Ok (Consumed_Result) then
                        Shutdown (Manager);
                        return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                           Kind        => Assertion_Failed,
                           Message     => To_Unbounded_String ("Should mark buffer consumed"),
                           Details     => Null_Unbounded_String,
                           Line_Number => 0,
                           Test_Name   => To_Unbounded_String ("Test_Consumer_Operations")
                        ));
                     end if;
                  end;

                  if Buffer_Count_In_State (Manager, Free) /= 4 then
                     Shutdown (Manager);
                     return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                        Kind        => Assertion_Failed,
                        Message     => To_Unbounded_String ("All buffers should be free after consumption"),
                        Details     => To_Unbounded_String ("Got: " & Buffer_Count_In_State (Manager, Free)'Image),
                        Line_Number => 0,
                        Test_Name   => To_Unbounded_String ("Test_Consumer_Operations")
                     ));
                  end if;
               end;
            end;
         end;
      end;

      Shutdown (Manager);
      return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Ok (True);
   end Test_Consumer_Operations;

--   Test ownership transfer
   function Test_Ownership_Transfer return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Result is
      Manager       : Buffer_Manager_Type;
      Test_Data     : constant String := "Transfer Test";
   begin
      Initialize (Manager, 1024);

--   Producer: Prepare buffer
      declare
         Get_Result : constant BM_Buffer_Result.Result := Get_Free_Buffer (Manager);
      begin
         if not BM_Buffer_Result.Is_Ok (Get_Result) then
            Shutdown (Manager);
            return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Should get buffer for transfer test"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Ownership_Transfer")
            ));
         end if;

         declare
            Producer_Info : constant Buffer_Info := BM_Buffer_Result.Get_Ok (Get_Result);
         begin
--   Fill with test data
            for I in Test_Data'Range loop
               Producer_Info.Buffer (Stream_Element_Offset (I - Test_Data'First + 1)) :=
                 Stream_Element (Character'Pos (Test_Data (I)));
            end loop;

            declare
               Mark_Result : constant BM_Void_Result.Result :=
                 Mark_Buffer_Ready (Manager, Producer_Info.Index, Test_Data'Length);
            begin
               if not BM_Void_Result.Is_Ok (Mark_Result) then
                  Shutdown (Manager);
                  return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Should mark buffer ready"),
                     Details     => Null_Unbounded_String,
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Ownership_Transfer")
                  ));
               end if;
            end;

--   Consumer: Transfer ownership
            declare
               Transfer_Result : constant BM_Buffer_Result.Result := Try_Transfer_Buffer_Ownership (Manager);
            begin
               if not BM_Buffer_Result.Is_Ok (Transfer_Result) then
                  Shutdown (Manager);
                  return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Should transfer ownership successfully"),
                     Details     => Null_Unbounded_String,
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Ownership_Transfer")
                  ));
               end if;

               declare
                  Transfer_Info : constant Buffer_Info := BM_Buffer_Result.Get_Ok (Transfer_Result);
                  Transfer_Buffer : Stream_Element_Array_Access := Transfer_Info.Buffer;
               begin
                  if Transfer_Buffer = null then
                     Shutdown (Manager);
                     return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                        Kind        => Assertion_Failed,
                        Message     => To_Unbounded_String ("Transferred buffer should not be null"),
                        Details     => Null_Unbounded_String,
                        Line_Number => 0,
                        Test_Name   => To_Unbounded_String ("Test_Ownership_Transfer")
                     ));
                  end if;

                  if Transfer_Info.Size /= Test_Data'Length then
                     declare
                        Return_Result : constant BM_Void_Result.Result :=
                          Return_Buffer (Manager, Transfer_Buffer, Transfer_Info.Index);
                        pragma Unreferenced (Return_Result);
                     begin
                        Shutdown (Manager);
                        return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                           Kind        => Assertion_Failed,
                           Message     => To_Unbounded_String ("Transfer data size should match"),
                           Details     => To_Unbounded_String ("Expected: " & Test_Data'Length'Image &
                                         ", Got: " & Transfer_Info.Size'Image),
                           Line_Number => 0,
                           Test_Name   => To_Unbounded_String ("Test_Ownership_Transfer")
                        ));
                     end;
                  end if;

--   Buffer pool should have allocated a new buffer
                  if Buffer_Count_In_State (Manager, Free) /= 4 then
                     declare
                        Return_Result : constant BM_Void_Result.Result :=
                          Return_Buffer (Manager, Transfer_Buffer, Transfer_Info.Index);
                        pragma Unreferenced (Return_Result);
                     begin
                        Shutdown (Manager);
                        return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                           Kind        => Assertion_Failed,
                           Message     => To_Unbounded_String ("Pool should have new buffer after transfer"),
                           Details     => To_Unbounded_String ("Free count: " &
                                                          Buffer_Count_In_State (Manager, Free)'Image),
                           Line_Number => 0,
                           Test_Name   => To_Unbounded_String ("Test_Ownership_Transfer")
                        ));
                     end;
                  end if;

--   Verify transferred data
                  declare
                     Retrieved_Data : String (1 .. Transfer_Info.Size);
                  begin
                     for I in 1 .. Transfer_Info.Size loop
                        Retrieved_Data (I) := Character'Val (Transfer_Buffer (Stream_Element_Offset (I)));
                     end loop;

                     if Retrieved_Data /= Test_Data then
                        declare
                           Return_Result : constant BM_Void_Result.Result :=
                             Return_Buffer (Manager, Transfer_Buffer, Transfer_Info.Index);
                           pragma Unreferenced (Return_Result);
                        begin
                           Shutdown (Manager);
                           return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                              Kind        => Assertion_Failed,
                              Message     => To_Unbounded_String ("Transferred data should match"),
                              Details     => To_Unbounded_String ("Expected: '" & Test_Data &
                                            "', Got: '" & Retrieved_Data & "'"),
                              Line_Number => 0,
                              Test_Name   => To_Unbounded_String ("Test_Ownership_Transfer")
                           ));
                        end;
                     end if;
                  end;

--   Return transferred buffer
                  declare
                     Return_Result : constant BM_Void_Result.Result :=
                       Return_Buffer (Manager, Transfer_Buffer, Transfer_Info.Index);
                  begin
                     if not BM_Void_Result.Is_Ok (Return_Result) then
                        Shutdown (Manager);
                        return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                           Kind        => Assertion_Failed,
                           Message     => To_Unbounded_String ("Should return buffer successfully"),
                           Details     => Null_Unbounded_String,
                           Line_Number => 0,
                           Test_Name   => To_Unbounded_String ("Test_Ownership_Transfer")
                        ));
                     end if;
                  end;
                  if Transfer_Buffer /= null then
                     Shutdown (Manager);
                     return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                        Kind        => Assertion_Failed,
                        Message     => To_Unbounded_String ("Transfer buffer should be null after return"),
                        Details     => Null_Unbounded_String,
                        Line_Number => 0,
                        Test_Name   => To_Unbounded_String ("Test_Ownership_Transfer")
                     ));
                  end if;
               end;
            end;
         end;
      end;

      Shutdown (Manager);
      return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Ok (True);
   end Test_Ownership_Transfer;

--   Test error handling
   function Test_Error_Handling return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Result is
      Manager : Buffer_Manager_Type;
   begin
      Initialize (Manager, 1024);

--   Get buffer and mark as error
      declare
         Get_Result : constant BM_Buffer_Result.Result := Get_Free_Buffer (Manager);
      begin
         if not BM_Buffer_Result.Is_Ok (Get_Result) then
            Shutdown (Manager);
            return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Should get buffer for error test"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Error_Handling")
            ));
         end if;

         declare
            Info : constant Buffer_Info := BM_Buffer_Result.Get_Ok (Get_Result);
         begin
--   Mark buffer as having error
            declare
               Error_Result : constant BM_Void_Result.Result := Mark_Buffer_Error (Manager, Info.Index);
            begin
               if not BM_Void_Result.Is_Ok (Error_Result) then
                  Shutdown (Manager);
                  return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Should mark buffer as error"),
                     Details     => Null_Unbounded_String,
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Error_Handling")
                  ));
               end if;
            end;

            if Buffer_Count_In_State (Manager, Error) /= 1 then
               Shutdown (Manager);
               return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Should have 1 error buffer"),
                  Details     => To_Unbounded_String ("Got: " & Buffer_Count_In_State (Manager, Error)'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Error_Handling")
               ));
            end if;

--   Check that error buffer is not available for consumption
            if Has_Ready_Buffer (Manager) then
               Shutdown (Manager);
               return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Error buffer should not be ready"),
                  Details     => Null_Unbounded_String,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Error_Handling")
               ));
            end if;

--   Verify statistics reflect error state
            declare
               Stats : constant Buffer_Statistics_Type := Get_Statistics (Manager);
            begin
               if Stats.Error_Buffers_Count /= 1 then
                  Shutdown (Manager);
                  return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Statistics should show 1 error buffer"),
                     Details     => To_Unbounded_String ("Got: " & Stats.Error_Buffers_Count'Image),
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Error_Handling")
                  ));
               end if;
            end;
         end;
      end;

      Shutdown (Manager);
      return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Ok (True);
   end Test_Error_Handling;

--   Test shutdown and cleanup
   function Test_Shutdown return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Result is
      Manager : Buffer_Manager_Type;
   begin
      Initialize (Manager, 512);

      if not Is_Initialized (Manager) then
         return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Manager should be initialized"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Shutdown")
         ));
      end if;

--   Shutdown
      Shutdown (Manager);

      if Is_Initialized (Manager) then
         return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Manager should not be initialized after shutdown"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Shutdown")
         ));
      end if;

      if not Is_Shutdown (Manager) then
         return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Manager should be shut down"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Shutdown")
         ));
      end if;

      return Abohlib.Infrastructure.Testing.Test_Framework.Void_Result.Ok (True);
   end Test_Shutdown;

--   Run all tests
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Test_Count : constant := 6;
      Tests : Test_Results_Array (1 .. Test_Count);
      procedure Run_Single_Test
        (Index : Positive;
         Name : String;
         Func : Test_Function_Access) is
         Result : constant Test_Result_Pkg.Result := Run_Test (Name, Func, Output);
      begin
         if Result.Is_Ok then
            Tests (Index) := Result.Get_Ok;
            Print_Test_Result (Tests (Index), Output);
         else
--  For simplicity, we'll create a failed test result
            Tests (Index) := Test_Result'(
               Name => To_Unbounded_String (Name),
               Status => Error,
               Message => To_Unbounded_String ("Test execution failed"),
               Elapsed_Time => 0.0,
               Line_Number => 0,
               Correlation_ID => Null_Unbounded_String
            );
            Print_Test_Result (Tests (Index), Output);
         end if;
      end Run_Single_Test;
   begin
      Output.Write_Line ("=== Running Concurrent Buffer Manager Tests ===");
      Run_Single_Test (1, "Initialization", Test_Initialization'Access);
      Run_Single_Test (2, "Producer Operations", Test_Producer_Operations'Access);
      Run_Single_Test (3, "Consumer Operations", Test_Consumer_Operations'Access);
      Run_Single_Test (4, "Ownership Transfer", Test_Ownership_Transfer'Access);
      Run_Single_Test (5, "Error Handling", Test_Error_Handling'Access);
      Run_Single_Test (6, "Shutdown", Test_Shutdown'Access);
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Concurrent Buffer Manager", Tests, Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Concurrent Buffer Manager Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Buffer_Manager;

pragma Warnings (On, "subprogram body has no previous spec");
