--  =============================================================================
--  Abohlib.Core.Domain.Utilities.IO.Generic_Zero_Copy - Generic Zero-Copy I/O Interface
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Generic interface for zero-copy I/O operations providing platform-specific
--  optimized data transfer methods including memory mapping, sendfile, splice,
--  and scatter-gather I/O.
--  =============================================================================

pragma Ada_2022;

with System; use System;
with Ada.Streams; use Ada.Streams;
with Abohlib.Core.Domain.Types.Bytes; use Abohlib.Core.Domain.Types.Bytes;
with Abohlib.Core.Domain.Constants.Bytes;

generic
   --  File descriptor or handle type
   type File_Handle_Type is private;
   
   --  Result types for operations
   type Result_Type is private;
   type Error_Type is private;
   
   --  Result construction functions
   with function Make_Ok_Result (Bytes : SI_Bytes_Type) return Result_Type;
   with function Make_Err_Result (Error : Error_Type) return Result_Type;
   with function Is_Ok (Result : Result_Type) return Boolean;
   
   --  Error constants
   Err_Not_Supported : Error_Type;
   Err_File_Open : Error_Type;
   Err_File_Size : Error_Type;
   Err_Transfer : Error_Type;
   Err_Dest_Too_Small : Error_Type;
   Err_Copy_Failed : Error_Type;
   
   --  Platform-specific file operations
   with function Open_File (Path : String; Mode : Natural) return File_Handle_Type;
   with procedure Close_File (Handle : File_Handle_Type);
   with function Get_File_Size (Handle : File_Handle_Type) return SI_Bytes_Type;
   with function Is_Valid_Handle (Handle : File_Handle_Type) return Boolean;
   
package Abohlib.Core.Domain.Utilities.IO.Generic_Zero_Copy is

   pragma Elaborate_Body;

   --  Transfer methods available
   type Transfer_Method_Type is 
     (Sendfile,      -- Linux sendfile()
      Copyfile,      -- macOS copyfile()
      TransmitFile,  -- Windows TransmitFile()
      Splice,        -- Linux splice()
      MemoryMap,     -- mmap() based
      LargeBuffer);  -- Fallback with large buffers

   --  Transfer mode for memory mapping
   type Map_Mode_Type is (Read_Only, Write_Only, Read_Write);
      
   --  Get best available method for current platform
   function Best_Method return Transfer_Method_Type;
   
   --  Check if method is supported on current platform
   function Is_Supported (Method : Transfer_Method_Type) return Boolean;

   --  Generic zero-copy file transfer
   generic
      with function Platform_Transfer 
        (Source      : File_Handle_Type;
         Destination : File_Handle_Type;
         Offset      : SI_Bytes_Type;
         Count       : SI_Bytes_Type) return Result_Type;
   function Zero_Copy_Transfer
     (Source_Path : String;
      Dest_Path   : String;
      Method      : Transfer_Method_Type := Best_Method) return Result_Type
   with
     Pre => Source_Path'Length > 0 and then Dest_Path'Length > 0,
     Post => (if Is_Ok (Zero_Copy_Transfer'Result) then 
               Source_Path /= Dest_Path else True);
   
   --  Generic memory-mapped I/O
   generic
      type Mapped_Region_Type is private;
      
      with function Map_File 
        (Handle : File_Handle_Type;
         Offset : SI_Bytes_Type;
         Length : SI_Bytes_Type;
         Mode   : Map_Mode_Type) return Mapped_Region_Type;
         
      with procedure Unmap 
        (Region : Mapped_Region_Type);
        
      with function Get_Address 
        (Region : Mapped_Region_Type) return System.Address;

      with function Is_Valid_Region 
        (Region : Mapped_Region_Type) return Boolean;
        
   package Memory_Mapped_IO is
      
      type Mapped_File_Type is tagged private;
      
      --  Open and map file for zero-copy access
      function Open 
        (Path   : String;
         Mode   : Map_Mode_Type := Read_Only) 
         return Mapped_File_Type
      with
        Pre => Path'Length > 0,
        Post => (if Open'Result.Is_Valid then Open'Result.Get_Size >= 0 else True);
         
      --  Get mapped data as stream elements (zero-copy view)
      function As_Stream_Elements 
        (File : Mapped_File_Type) return Stream_Element_Array
      with
        Pre => File.Is_Valid,
        Post => As_Stream_Elements'Result'Length = Natural (File.Get_Size);
        
      --  Copy between mapped files (platform-optimized)
      function Copy 
        (Source : Mapped_File_Type;
         Dest   : in out Mapped_File_Type;
         Offset : SI_Bytes_Type := 0;
         Count  : SI_Bytes_Type := 0) -- 0 = entire file
         return Result_Type
      with
        Pre => Source.Is_Valid and then Dest.Is_Valid,
        Post => Is_Ok (Copy'Result) or else not Is_Ok (Copy'Result);
         
      --  Close and unmap file
      procedure Close (File : in out Mapped_File_Type)
      with
        Pre => File.Is_Valid,
        Post => not File.Is_Valid;

      --  Check if file is mapped and valid
      function Is_Valid (File : Mapped_File_Type) return Boolean;

      --  Get file size
      function Get_Size (File : Mapped_File_Type) return SI_Bytes_Type
      with
        Pre => File.Is_Valid,
        Post => Get_Size'Result >= 0;

      --  Get memory address for advanced operations
      function Get_Address (File : Mapped_File_Type) return System.Address
      with Pre => File.Is_Valid;
      
   private
      
      type Mapped_File_Type is tagged record
         Handle    : File_Handle_Type;
         Region    : Mapped_Region_Type;
         Size      : SI_Bytes_Type := 0;
         Is_Mapped : Boolean := False;
         Mode      : Map_Mode_Type := Read_Only;
      end record;
      
   end Memory_Mapped_IO;
   
   --  I/O vector type for scatter-gather operations
   type IO_Vector_Type is record
      Base   : System.Address;
      Length : Natural;
   end record;
   
   --  Generic scatter-gather I/O for vectored operations
   generic
      Max_Segments_Count : Positive := 16;
      
      type IO_Vector_Array is array (Positive range <>) of IO_Vector_Type;
      
      with function Readv 
        (Handle  : File_Handle_Type;
         Vectors : IO_Vector_Array;
         Count   : Positive) return Result_Type;
         
      with function Writev 
        (Handle  : File_Handle_Type;
         Vectors : IO_Vector_Array;
         Count   : Positive) return Result_Type;
         
   package Scatter_Gather_IO is
      
      type SG_Buffer_Type is tagged private;
      
      --  Add memory segment to scatter-gather buffer
      procedure Add_Segment 
        (Buffer  : in out SG_Buffer_Type;
         Address : System.Address;
         Length  : Natural)
      with
        Pre => Address /= System.Null_Address and then 
               Length > 0 and then 
               Buffer.Get_Segment_Count < Max_Segments_Count,
        Post => Buffer.Get_Segment_Count = Buffer.Get_Segment_Count'Old + 1;
         
      --  Read into scatter-gather buffer (vectored read)
      function Read 
        (Buffer : in out SG_Buffer_Type;
         Handle : File_Handle_Type) return Result_Type
      with
        Pre => Buffer.Get_Segment_Count > 0 and then Is_Valid_Handle (Handle);
         
      --  Write from scatter-gather buffer (vectored write)
      function Write 
        (Buffer : SG_Buffer_Type;
         Handle : File_Handle_Type) return Result_Type
      with
        Pre => Buffer.Get_Segment_Count > 0 and then Is_Valid_Handle (Handle);
         
      --  Clear all segments
      procedure Clear (Buffer : in out SG_Buffer_Type)
      with Post => Buffer.Get_Segment_Count = 0;

      --  Get number of segments in buffer
      function Get_Segment_Count (Buffer : SG_Buffer_Type) return Natural;

      --  Get total size of all segments
      function Get_Total_Size (Buffer : SG_Buffer_Type) return Natural;
      
   private
      
      subtype IO_Vector_Array_Type is IO_Vector_Array (1 .. Max_Segments_Count);
      
      type SG_Buffer_Type is tagged record
         Vectors      : IO_Vector_Array_Type;
         Num_Segments : Natural := 0;
      end record;
      
   end Scatter_Gather_IO;

   --  Direct buffer operations for high-performance scenarios
   generic
      Buffer_Size_Bytes : Positive := Natural (Abohlib.Core.Domain.Constants.Bytes.SI_MB_LLI);  -- 1MB default
   package Direct_Buffer_IO is

      type Direct_Buffer_Type is tagged private;

      --  Initialize direct buffer with alignment
      procedure Initialize (Buffer : in out Direct_Buffer_Type)
      with Post => Buffer.Is_Valid;

      --  Read directly into aligned buffer
      function Read_Direct
        (Buffer : in out Direct_Buffer_Type;
         Handle : File_Handle_Type;
         Count  : Natural) return Result_Type
      with
        Pre => Buffer.Is_Valid and then Is_Valid_Handle (Handle) and then
               Count <= Buffer_Size_Bytes;

      --  Write directly from aligned buffer  
      function Write_Direct
        (Buffer : Direct_Buffer_Type;
         Handle : File_Handle_Type;
         Count  : Natural) return Result_Type
      with
        Pre => Buffer.Is_Valid and then Is_Valid_Handle (Handle) and then
               Count <= Buffer_Size_Bytes;

      --  Get buffer as stream elements for processing
      function As_Stream_Elements
        (Buffer : Direct_Buffer_Type;
         Count  : Natural) return Stream_Element_Array
      with
        Pre => Buffer.Is_Valid and then Count <= Buffer_Size_Bytes,
        Post => As_Stream_Elements'Result'Length = Count;

      --  Check if buffer is valid
      function Is_Valid (Buffer : Direct_Buffer_Type) return Boolean;

      --  Get buffer capacity
      function Get_Capacity (Buffer : Direct_Buffer_Type) return Natural
      is (Buffer_Size_Bytes);

      --  Cleanup direct buffer
      procedure Finalize (Buffer : in out Direct_Buffer_Type)
      with Post => not Buffer.Is_Valid;

   private

      type Direct_Buffer_Type is tagged record
         Data    : System.Address := System.Null_Address;
         Valid   : Boolean := False;
      end record;

   end Direct_Buffer_IO;

end Abohlib.Core.Domain.Utilities.IO.Generic_Zero_Copy;