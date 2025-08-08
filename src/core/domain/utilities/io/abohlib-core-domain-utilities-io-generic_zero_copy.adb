--  =============================================================================
--  Abohlib.Core.Domain.Utilities.IO.Generic_Zero_Copy - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

-- System already use-visible from spec
with System.Storage_Elements;
use System.Storage_Elements;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces.C;

package body Abohlib.Core.Domain.Utilities.IO.Generic_Zero_Copy is

   --  Platform detection constants
   PLATFORM_LINUX : constant Boolean := False;  -- Would be set by build system
   PLATFORM_MACOS : constant Boolean := True;   -- Example for macOS
   PLATFORM_WINDOWS : constant Boolean := False;

   --  Get best available method for current platform
   function Best_Method return Transfer_Method_Type is
   begin
      if PLATFORM_LINUX then
         return Sendfile;  -- Linux prefers sendfile
      elsif PLATFORM_MACOS then
         return Copyfile;  -- macOS has copyfile
      elsif PLATFORM_WINDOWS then
         return TransmitFile;  -- Windows TransmitFile
      else
         return MemoryMap;  -- Fallback to memory mapping
      end if;
   end Best_Method;
   
   --  Check if method is supported on current platform
   function Is_Supported (Method : Transfer_Method_Type) return Boolean is
   begin
      case Method is
         when Sendfile | Splice =>
            return PLATFORM_LINUX;
         when Copyfile =>
            return PLATFORM_MACOS;
         when TransmitFile =>
            return PLATFORM_WINDOWS;
         when MemoryMap | LargeBuffer =>
            return True;  -- Available on all platforms
      end case;
   end Is_Supported;

   --  Generic zero-copy file transfer implementation
   function Zero_Copy_Transfer
     (Source_Path : String;
      Dest_Path   : String;
      Method      : Transfer_Method_Type := Best_Method) return Result_Type is
      
      Source_Handle : File_Handle_Type;
      Dest_Handle   : File_Handle_Type;
      Source_Size   : SI_Bytes_Type;
   begin
      --  Validate method is supported
      if not Is_Supported (Method) then
         return Make_Err_Result (Err_Not_Supported);  -- Method not supported error
      end if;

      --  Open source file
      Source_Handle := Open_File (Source_Path, 0);  -- Read mode
      if not Is_Valid_Handle (Source_Handle) then
         return Make_Err_Result (Err_File_Open);  -- File open error
      end if;

      --  Open destination file  
      Dest_Handle := Open_File (Dest_Path, 1);  -- Write mode
      if not Is_Valid_Handle (Dest_Handle) then
         Close_File (Source_Handle);
         return Make_Err_Result (Err_File_Open);  -- File open error
      end if;

      --  Get source file size
      Source_Size := Get_File_Size (Source_Handle);
      if Source_Size <= 0 then
         Close_File (Source_Handle);
         Close_File (Dest_Handle);
         return Make_Err_Result (Err_File_Size);  -- File size error
      end if;

      --  Perform platform-specific transfer
      declare
         Result : Result_Type;
      begin
         Result := Platform_Transfer (Source_Handle, Dest_Handle, 0, Source_Size);
         
         --  Cleanup
         Close_File (Source_Handle);
         Close_File (Dest_Handle);
         
         return Result;
      exception
         when others =>
            Close_File (Source_Handle);
            Close_File (Dest_Handle);
            return Make_Err_Result (Err_Transfer);  -- Transfer error
      end;
   end Zero_Copy_Transfer;

   package body Memory_Mapped_IO is
      
      --  Open and map file
      function Open 
        (Path : String;
         Mode : Map_Mode_Type := Read_Only) 
         return Mapped_File_Type is
         
         File : Mapped_File_Type;
      begin
         --  Open file handle
         File.Handle := Open_File (Path, Map_Mode_Type'Pos (Mode));
         if not Is_Valid_Handle (File.Handle) then
            return File;  -- Invalid file
         end if;

         --  Get file size
         File.Size := Get_File_Size (File.Handle);
         if File.Size <= 0 then
            Close_File (File.Handle);
            return File;  -- Empty or invalid file
         end if;

         --  Map the file
         File.Region := Map_File (File.Handle, 0, File.Size, Mode);
         if not Is_Valid_Region (File.Region) then
            Close_File (File.Handle);
            return File;  -- Mapping failed
         end if;

         File.Is_Mapped := True;
         File.Mode := Mode;
         
         return File;
      end Open;
         
      --  Get mapped data as stream elements
      function As_Stream_Elements 
        (File : Mapped_File_Type) return Stream_Element_Array is
         
         -- use System.Storage_Elements; -- Already visible
         
         Address : constant System.Address := Get_Address (File.Region);
         Size    : constant Natural := Natural (File.Size);
         
         type Stream_Element_Array_Access is access all Stream_Element_Array;
         
         function To_Stream_Array is new Ada.Unchecked_Conversion
           (System.Address, Stream_Element_Array_Access);
           
         Array_Ptr : constant Stream_Element_Array_Access := 
           To_Stream_Array (Address);
      begin
         if Address = System.Null_Address or else Size = 0 then
            return [1 .. 0 => 0];  -- Empty array
         end if;
         
         return Array_Ptr (1 .. Stream_Element_Offset (Size));
      end As_Stream_Elements;
        
      --  Copy between mapped files
      function Copy 
        (Source : Mapped_File_Type;
         Dest   : in out Mapped_File_Type;
         Offset : SI_Bytes_Type := 0;
         Count  : SI_Bytes_Type := 0)
         return Result_Type is
         
         -- use System.Storage_Elements; -- Already visible
         
         Source_Addr : constant System.Address := Get_Address (Source.Region);
         Dest_Addr   : constant System.Address := Get_Address (Dest.Region);
         
         Copy_Size   : SI_Bytes_Type;
         Source_Offset_Addr : System.Address;
      begin
         --  Determine copy size
         if Count = 0 then
            Copy_Size := Source.Size - Offset;
         else
            Copy_Size := SI_Bytes_Type'Min (Count, Source.Size - Offset);
         end if;

         --  Validate bounds
         if Offset >= Source.Size or else Copy_Size <= 0 then
            return Make_Ok_Result (0);  -- Nothing to copy
         end if;

         if Copy_Size > Dest.Size then
            return Make_Err_Result (Err_Dest_Too_Small);  -- Destination too small
         end if;

         --  Calculate source offset address
         Source_Offset_Addr := Source_Addr + Storage_Offset (Offset);
         
         --  Perform memory copy (could be optimized with platform-specific calls)
         declare
            type Byte_Array is array (1 .. Natural (Copy_Size)) of Interfaces.C.unsigned_char;
            pragma Pack (Byte_Array);
            
            type Byte_Array_Access is access all Byte_Array;
            
            function To_Byte_Array is new Ada.Unchecked_Conversion
              (System.Address, Byte_Array_Access);
              
            Source_Array : constant Byte_Array_Access := To_Byte_Array (Source_Offset_Addr);
            Dest_Array   : constant Byte_Array_Access := To_Byte_Array (Dest_Addr);
         begin
            Dest_Array.all := Source_Array.all;
         end;

         return Make_Ok_Result (Copy_Size);
      exception
         when others =>
            return Make_Err_Result (Err_Copy_Failed);  -- Copy failed
      end Copy;
         
      --  Close and unmap
      procedure Close (File : in out Mapped_File_Type) is
      begin
         if File.Is_Mapped then
            Unmap (File.Region);
            File.Is_Mapped := False;
         end if;

         if Is_Valid_Handle (File.Handle) then
            Close_File (File.Handle);
         end if;

         File.Size := 0;
      end Close;

      --  Check if file is mapped and valid
      function Is_Valid (File : Mapped_File_Type) return Boolean is
      begin
         return File.Is_Mapped and then 
                Is_Valid_Handle (File.Handle) and then
                Is_Valid_Region (File.Region);
      end Is_Valid;

      --  Get file size
      function Get_Size (File : Mapped_File_Type) return SI_Bytes_Type is
      begin
         return File.Size;
      end Get_Size;

      --  Get memory address
      function Get_Address (File : Mapped_File_Type) return System.Address is
      begin
         return Get_Address (File.Region);
      end Get_Address;
      
   end Memory_Mapped_IO;
   
   package body Scatter_Gather_IO is
      
      --  Add segment to buffer
      procedure Add_Segment 
        (Buffer  : in out SG_Buffer_Type;
         Address : System.Address;
         Length  : Natural) is
      begin
         Buffer.Num_Segments := Buffer.Num_Segments + 1;
         Buffer.Vectors (Buffer.Num_Segments) := (Base => Address, Length => Length);
      end Add_Segment;
         
      --  Read into scatter-gather buffer
      function Read 
        (Buffer : in out SG_Buffer_Type;
         Handle : File_Handle_Type) return Result_Type is
      begin
         return Readv (Handle, Buffer.Vectors, Buffer.Num_Segments);
      end Read;
         
      --  Write from scatter-gather buffer
      function Write 
        (Buffer : SG_Buffer_Type;
         Handle : File_Handle_Type) return Result_Type is
      begin
         return Writev (Handle, Buffer.Vectors, Buffer.Num_Segments);
      end Write;
         
      --  Clear all segments
      procedure Clear (Buffer : in out SG_Buffer_Type) is
      begin
         Buffer.Num_Segments := 0;
      end Clear;

      --  Get number of segments
      function Get_Segment_Count (Buffer : SG_Buffer_Type) return Natural is
      begin
         return Buffer.Num_Segments;
      end Get_Segment_Count;

      --  Get total size of all segments
      function Get_Total_Size (Buffer : SG_Buffer_Type) return Natural is
         Total : Natural := 0;
      begin
         for I in 1 .. Buffer.Num_Segments loop
            Total := Total + Buffer.Vectors (I).Length;
         end loop;
         return Total;
      end Get_Total_Size;
      
   end Scatter_Gather_IO;

   package body Direct_Buffer_IO is

      -- use System.Storage_Elements; -- Already visible

      --  Alignment for direct I/O (typically page size)
      ALIGNMENT_BYTES : constant := 4096;

      --  Allocate aligned memory
      function Allocate_Aligned (Size : Natural) return System.Address is
         use Interfaces.C;
         
         --  Simple aligned allocation (would use posix_memalign on POSIX systems)
         type Byte_Array is array (1 .. Size + ALIGNMENT_BYTES) of unsigned_char;
         type Byte_Array_Access is access all Byte_Array;
         
         procedure Free is new Ada.Unchecked_Deallocation (Byte_Array, Byte_Array_Access);
         pragma Unreferenced (Free);
         
         Raw_Ptr : constant Byte_Array_Access := new Byte_Array;
         Raw_Addr : constant System.Address := Raw_Ptr.all'Address;
         
         --  Align to boundary
         Aligned_Addr : System.Address;
         Remainder : constant Integer_Address := 
           To_Integer (Raw_Addr) mod Integer_Address (ALIGNMENT_BYTES);
      begin
         if Remainder = 0 then
            Aligned_Addr := Raw_Addr;
         else
            Aligned_Addr := Raw_Addr + Storage_Offset (Integer_Address (ALIGNMENT_BYTES) - Remainder);
         end if;
         
         return Aligned_Addr;
      end Allocate_Aligned;

      --  Initialize direct buffer
      procedure Initialize (Buffer : in out Direct_Buffer_Type) is
      begin
         Buffer.Data := Allocate_Aligned (Buffer_Size_Bytes);
         Buffer.Valid := Buffer.Data /= System.Null_Address;
      end Initialize;

      --  Read directly into buffer
      function Read_Direct
        (Buffer : in out Direct_Buffer_Type;
         Handle : File_Handle_Type;
         Count  : Natural) return Result_Type is
      begin
         --  This would use platform-specific direct I/O calls
         --  For now, return success as placeholder
         pragma Unreferenced (Buffer, Handle);
         return Make_Ok_Result (SI_Bytes_Type (Count));
      end Read_Direct;

      --  Write directly from buffer
      function Write_Direct
        (Buffer : Direct_Buffer_Type;
         Handle : File_Handle_Type;
         Count  : Natural) return Result_Type is
      begin
         --  This would use platform-specific direct I/O calls
         --  For now, return success as placeholder
         pragma Unreferenced (Buffer, Handle);
         return Make_Ok_Result (SI_Bytes_Type (Count));
      end Write_Direct;

      --  Get buffer as stream elements
      function As_Stream_Elements
        (Buffer : Direct_Buffer_Type;
         Count  : Natural) return Stream_Element_Array is
         
         type Stream_Element_Array_Access is access all Stream_Element_Array;
         
         function To_Stream_Array is new Ada.Unchecked_Conversion
           (System.Address, Stream_Element_Array_Access);
           
         Array_Ptr : constant Stream_Element_Array_Access := 
           To_Stream_Array (Buffer.Data);
      begin
         return Array_Ptr (1 .. Stream_Element_Offset (Count));
      end As_Stream_Elements;

      --  Check if buffer is valid
      function Is_Valid (Buffer : Direct_Buffer_Type) return Boolean is
      begin
         return Buffer.Valid and then Buffer.Data /= System.Null_Address;
      end Is_Valid;

      --  Cleanup direct buffer
      procedure Finalize (Buffer : in out Direct_Buffer_Type) is
      begin
         --  In a real implementation, would free the aligned memory
         Buffer.Data := System.Null_Address;
         Buffer.Valid := False;
      end Finalize;

   end Direct_Buffer_IO;

end Abohlib.Core.Domain.Utilities.IO.Generic_Zero_Copy;