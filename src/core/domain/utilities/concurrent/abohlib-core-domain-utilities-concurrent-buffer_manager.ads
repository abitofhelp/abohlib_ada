--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Concurrent.Buffer_Manager - Generic Buffer Manager
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Generic double-buffered manager for producer-consumer patterns.
--  Provides zero-copy buffer ownership transfers with thread safety.
--  Implements RAII principles and comprehensive error handling.
--  =============================================================================

pragma Ada_2022;

with Ada.Streams; use Ada.Streams;
with Ada.Finalization;
with Abohlib.Core.Domain.Constants.Concurrent;
with Abohlib.Core.Domain.Errors;
with Abohlib.Core.Domain.Result;

generic
   Buffer_Count_Value : Positive := Abohlib.Core.Domain.Constants.Concurrent.Default_Buffer_Count;
   Default_Buffer_Size_Bytes : Positive := Abohlib.Core.Domain.Constants.Concurrent.Default_Buffer_Size_Bytes;
   
package Abohlib.Core.Domain.Utilities.Concurrent.Buffer_Manager is

   --  Stream element array access type for buffers
   type Stream_Element_Array_Access is access all Stream_Element_Array;

   --  Buffer lifecycle states
   type Buffer_State_Type is 
     (Free,       -- Available for producer use
      Reading,    -- Producer is filling buffer
      Ready,      -- Buffer contains data, ready for consumer
      Consuming,  -- Consumer is processing buffer
      Error);     -- Buffer is in error state

   --  Buffer statistics for monitoring
   type Buffer_Statistics_Type is record
      Total_Buffers_Count    : Natural := 0;
      Free_Buffers_Count     : Natural := 0;
      Reading_Buffers_Count  : Natural := 0;
      Ready_Buffers_Count    : Natural := 0;
      Consuming_Buffers_Count : Natural := 0;
      Error_Buffers_Count    : Natural := 0;
      Buffer_Size_Bytes      : Natural := 0;
   end record;

   --  Buffer manager with automatic resource management
   type Buffer_Manager_Type is limited new Ada.Finalization.Limited_Controlled with private;
   --  Note: Type_Invariant would be ideal here to ensure:
   --    Sum of all buffer counts equals Total_Buffers_Count
   --  However, Ada doesn't allow Type_Invariants to access private components
   --  when the type is declared as private. The invariant that the sum of
   --  Free + Reading + Ready + Consuming + Error buffers equals the total
   --  is maintained internally by the protected operations.
   
   --  Error types for buffer operations
   type Buffer_Error is record
      Base : Abohlib.Core.Domain.Errors.Domain_Error;
   end record;
   
   --  Buffer info type for results
   type Buffer_Info is record
      Buffer : Stream_Element_Array_Access;
      Size   : Natural;
      Index  : Natural;
   end record;
   
   --  Result types
   package Buffer_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Buffer_Info, Err_Type => Buffer_Error);
   
   package Void_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Boolean, Err_Type => Buffer_Error);

   --  Initialize buffer manager with specified buffer size
   procedure Initialize 
     (Manager     : in out Buffer_Manager_Type;
      Buffer_Size : Natural := Default_Buffer_Size_Bytes)
   with
     Pre => Buffer_Size > 0,
     Post => Manager.Is_Initialized and then 
             Manager.Get_Statistics.Total_Buffers_Count = Buffer_Count_Value and then
             Manager.Get_Statistics.Buffer_Size_Bytes = Buffer_Size;

   --  Check if manager is initialized and operational
   function Is_Initialized (Manager : Buffer_Manager_Type) return Boolean;

   --  Check if manager is shut down
   function Is_Shutdown (Manager : Buffer_Manager_Type) return Boolean
   with Post => (if Is_Shutdown'Result then not Manager.Is_Initialized else True);

   --  Producer operations: Acquire buffer for writing
   function Get_Free_Buffer 
     (Manager : in out Buffer_Manager_Type) return Buffer_Result.Result
   with
     Pre => Manager.Is_Initialized and then not Manager.Is_Shutdown,
     Post => (if Buffer_Result.Is_Ok (Get_Free_Buffer'Result) then
                Buffer_Result.Get_Ok (Get_Free_Buffer'Result).Buffer /= null);

   --  Producer operations: Mark buffer as ready for consumption
   function Mark_Buffer_Ready 
     (Manager : in out Buffer_Manager_Type;
      Index   : Natural; 
      Size    : Natural) return Void_Result.Result
   with
     Pre => Manager.Is_Initialized and then not Manager.Is_Shutdown and then
            Index < Buffer_Count_Value and then Size > 0,
     Post => (if Void_Result.Is_Ok (Mark_Buffer_Ready'Result) then
                Manager.Has_Ready_Buffer);

   --  Producer operations: Mark buffer as having an error
   function Mark_Buffer_Error 
     (Manager : in out Buffer_Manager_Type;
      Index   : Natural) return Void_Result.Result
   with
     Pre => Manager.Is_Initialized and then not Manager.Is_Shutdown and then
            Index < Buffer_Count_Value;

   --  Consumer operations: Get buffer for reading (non-blocking)
   function Try_Get_Ready_Buffer
     (Manager : in out Buffer_Manager_Type) return Buffer_Result.Result
   with
     Pre => Manager.Is_Initialized and then not Manager.Is_Shutdown,
     Post => (if Buffer_Result.Is_Ok (Try_Get_Ready_Buffer'Result) then
                Buffer_Result.Get_Ok (Try_Get_Ready_Buffer'Result).Buffer /= null and then
                Buffer_Result.Get_Ok (Try_Get_Ready_Buffer'Result).Size > 0);

   --  Consumer operations: Transfer buffer ownership (zero-copy)
   function Try_Transfer_Buffer_Ownership
     (Manager : in out Buffer_Manager_Type) return Buffer_Result.Result
   with
     Pre => Manager.Is_Initialized and then not Manager.Is_Shutdown,
     Post => (if Buffer_Result.Is_Ok (Try_Transfer_Buffer_Ownership'Result) then
                Buffer_Result.Get_Ok (Try_Transfer_Buffer_Ownership'Result).Buffer /= null and then
                Buffer_Result.Get_Ok (Try_Transfer_Buffer_Ownership'Result).Size > 0);

   --  Consumer operations: Mark buffer as consumed
   function Mark_Buffer_Consumed 
     (Manager : in out Buffer_Manager_Type;
      Index   : Natural) return Void_Result.Result
   with
     Pre => Manager.Is_Initialized and then not Manager.Is_Shutdown and then
            Index < Buffer_Count_Value;

   --  Return transferred buffer to pool
   function Return_Buffer 
     (Manager : in out Buffer_Manager_Type;
      Buffer  : in out Stream_Element_Array_Access; 
      Index   : Natural) return Void_Result.Result
   with
     Pre => Manager.Is_Initialized and then Index < Buffer_Count_Value,
     Post => (if Void_Result.Is_Ok (Return_Buffer'Result) then Buffer = null);

   --  State query functions
   function Has_Ready_Buffer (Manager : Buffer_Manager_Type) return Boolean
   with Pre => Manager.Is_Initialized;

   function Has_Free_Buffer (Manager : Buffer_Manager_Type) return Boolean
   with Pre => Manager.Is_Initialized;

   function Buffer_Count_In_State 
     (Manager : Buffer_Manager_Type; 
      State   : Buffer_State_Type) return Natural
   with 
     Pre => Manager.Is_Initialized,
     Post => Buffer_Count_In_State'Result <= Buffer_Count_Value;

   --  Get comprehensive statistics
   function Get_Statistics (Manager : Buffer_Manager_Type) return Buffer_Statistics_Type
   with Pre => Manager.Is_Initialized;

   --  Get buffer capacity
   function Get_Buffer_Size (Manager : Buffer_Manager_Type) return Natural
   with 
     Pre => Manager.Is_Initialized,
     Post => Get_Buffer_Size'Result > 0;

   --  Shutdown buffer manager (releases all resources)
   procedure Shutdown (Manager : in out Buffer_Manager_Type)
   with
     Pre => Manager.Is_Initialized,
     Post => Manager.Is_Shutdown and then not Manager.Is_Initialized;

   --  RAII cleanup
   overriding
   procedure Finalize (Manager : in out Buffer_Manager_Type);

private

   --  Buffer entry with state and metadata
   type Buffer_Entry_Type is record
      Buffer : Stream_Element_Array_Access := null;
      Size   : Natural := 0;
      State  : Buffer_State_Type := Free;
   end record;

   --  Array of buffer entries
   type Buffer_Array_Type is array (0 .. Buffer_Count_Value - 1) of Buffer_Entry_Type;

   --  Protected buffer manager implementation
   protected type Protected_Manager_Type is

      --  Initialize all buffers with given size
      procedure Initialize_Buffers (Buffer_Size : Natural);
      
      --  Producer operations
      entry Get_Free_Buffer_Entry 
        (Buffer : out Stream_Element_Array_Access;
         Index  : out Natural);
      
      procedure Mark_Ready 
        (Index : Natural; 
         Size  : Natural);
      
      procedure Mark_Error (Index : Natural);
      
      --  Consumer operations  
      entry Get_Ready_Buffer_Entry
        (Buffer : out Stream_Element_Array_Access;
         Size   : out Natural;
         Index  : out Natural);
      
      entry Transfer_Ownership
        (Buffer : out Stream_Element_Array_Access;
         Size   : out Natural;
         Index  : out Natural);
      
      procedure Mark_Consumed (Index : Natural);
      
      --  Buffer management
      procedure Return_Transferred_Buffer 
        (Buffer : Stream_Element_Array_Access; 
         Index  : Natural);
      
      --  State queries
      function Ready_Available return Boolean;
      function Free_Available return Boolean;
      function Count_In_State (State : Buffer_State_Type) return Natural;
      function Get_Stats return Buffer_Statistics_Type;
      
      --  Lifecycle
      procedure Shutdown_Buffers;
      function Is_Active return Boolean;
      
   private
      Buffers             : Buffer_Array_Type;
      Is_Manager_Active   : Boolean := False;
      Buffer_Size_Value   : Natural := 0;
      
      --  Helper functions
      function Find_Buffer_In_State (State : Buffer_State_Type) return Natural;
      procedure Free_Buffer_Memory (Index : Natural);
      procedure Update_Buffer_State (Index : Natural; State : Buffer_State_Type);
      
   end Protected_Manager_Type;

   --  Main buffer manager type
   type Buffer_Manager_Type is limited new Ada.Finalization.Limited_Controlled with record
      Protected_Manager : Protected_Manager_Type;
      Manager_Ready     : Boolean := False;
   end record;

end Abohlib.Core.Domain.Utilities.Concurrent.Buffer_Manager;