--  =============================================================================
--  Abohlib.Core.Domain.Utilities.IO.Generic_Buffered - Generic Buffered I/O Framework
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Generic framework for buffered I/O operations providing efficient
--  reading and writing with configurable buffer sizes.
--  =============================================================================

pragma Ada_2022;

with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
-- Removed unused: with Abohlib.Core.Domain.Constants.Bytes;

generic
   --  Buffer size (default: 64 KB)
   Buffer_Size : Stream_Element_Offset := 64_000;
   
   --  Result type for operations
   type Result_Type is private;
   type Error_Type is private;
   
   with function Make_Ok (Bytes : Natural) return Result_Type;
   with function Make_Error (Error : Error_Type) return Result_Type;
   pragma Unreferenced (Make_Error);  -- Reserved for future error handling
   
package Abohlib.Core.Domain.Utilities.IO.Generic_Buffered is

   pragma Elaborate_Body;

   --  Generic buffered reader
   generic
      with procedure Read_Impl 
        (File : in out Ada.Streams.Stream_IO.File_Type;
         Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset);
   package Buffered_Reader is
      
      type Reader_Type is tagged private;
      
      --  Initialize reader with file
      procedure Initialize 
        (Reader : in out Reader_Type;
         File   : access Ada.Streams.Stream_IO.File_Type)
      with
        Pre => File /= null and then Ada.Streams.Stream_IO.Is_Open (File.all),
        Post => Reader.Is_Valid;
         
      --  Read data into buffer
      function Read 
        (Reader : in out Reader_Type;
         Data   : out Stream_Element_Array) return Result_Type
      with
        Pre => Reader.Is_Valid and then Data'Length > 0;
         
      --  Peek at buffered data without consuming
      function Peek 
        (Reader : Reader_Type;
         Count  : Stream_Element_Offset) return Stream_Element_Array
      with
        Pre => Reader.Is_Valid and then Count > 0;
         
      --  Skip bytes in buffer
      procedure Skip 
        (Reader : in out Reader_Type;
         Count  : Stream_Element_Offset)
      with
        Pre => Reader.Is_Valid and then Count > 0;

      --  Check if reader is valid (has open file)
      function Is_Valid (Reader : Reader_Type) return Boolean;

      --  Check if end of file reached
      function End_Of_File (Reader : Reader_Type) return Boolean
      with Pre => Reader.Is_Valid;

      --  Get bytes available in buffer
      function Available_Bytes (Reader : Reader_Type) return Natural
      with Pre => Reader.Is_Valid;
         
   private
      
      type File_Access is access all Ada.Streams.Stream_IO.File_Type;
      
      type Reader_Type is tagged record
         File        : File_Access := null;
         Buffer      : Stream_Element_Array (1 .. Buffer_Size);
         Buffer_Pos  : Stream_Element_Offset := 1;
         Buffer_End  : Stream_Element_Offset := 0;
         End_Of_File : Boolean := False;
         Valid       : Boolean := False;
      end record;
      
   end Buffered_Reader;
   
   --  Generic buffered writer
   generic
      with procedure Write_Impl 
        (File : in out Ada.Streams.Stream_IO.File_Type;
         Item : Stream_Element_Array);
      with procedure Flush_Impl 
        (File : in out Ada.Streams.Stream_IO.File_Type);
   package Buffered_Writer is
      
      type File_Access is access all Ada.Streams.Stream_IO.File_Type;
      
      type Writer_Type is tagged private;
      
      --  Initialize writer with file
      procedure Initialize 
        (Writer : in out Writer_Type;
         File   : access Ada.Streams.Stream_IO.File_Type)
      with
        Pre => File /= null and then Ada.Streams.Stream_IO.Is_Open (File.all),
        Post => Writer.Is_Valid;
         
      --  Write data to buffer
      function Write 
        (Writer : in out Writer_Type;
         Data   : Stream_Element_Array) return Result_Type
      with
        Pre => Writer.Is_Valid;
         
      --  Flush buffer to file
      function Flush 
        (Writer : in out Writer_Type) return Result_Type
      with
        Pre => Writer.Is_Valid;

      --  Check if writer is valid (has open file)
      function Is_Valid (Writer : Writer_Type) return Boolean;
         
      --  Get bytes written
      function Bytes_Written 
        (Writer : Writer_Type) return Long_Long_Integer
      with
        Pre => Writer.Is_Valid,
        Post => Bytes_Written'Result >= 0;

      --  Get bytes buffered (not yet flushed)
      function Buffered_Bytes (Writer : Writer_Type) return Natural
      with
        Pre => Writer.Is_Valid,
        Post => Buffered_Bytes'Result <= Natural (Buffer_Size);
         
   private
      
      type Writer_Type is tagged record
         File          : File_Access := null;
         Buffer        : Stream_Element_Array (1 .. Buffer_Size);
         Buffer_Used   : Stream_Element_Offset := 0;
         Total_Written : Long_Long_Integer := 0;
         Valid         : Boolean := False;
      end record;
      
   end Buffered_Writer;

end Abohlib.Core.Domain.Utilities.IO.Generic_Buffered;