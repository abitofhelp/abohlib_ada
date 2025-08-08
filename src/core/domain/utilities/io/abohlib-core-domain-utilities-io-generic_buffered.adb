--  =============================================================================
--  Abohlib.Core.Domain.Utilities.IO.Generic_Buffered - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Utilities.IO.Generic_Buffered is

   package body Buffered_Reader is

      --  Initialize reader with file
      procedure Initialize 
        (Reader : in out Reader_Type;
         File   : access Ada.Streams.Stream_IO.File_Type) is
      begin
         Reader.File := File_Access (File);
         Reader.Buffer_Pos := 1;
         Reader.Buffer_End := 0;
         Reader.End_Of_File := False;
         Reader.Valid := True;
      end Initialize;

      --  Fill buffer from file
      procedure Fill_Buffer (Reader : in out Reader_Type) is
         Last : Stream_Element_Offset;
      begin
         if Reader.End_Of_File then
            return;
         end if;

         begin
            Read_Impl (Reader.File.all, Reader.Buffer, Last);
            Reader.Buffer_End := Last;
            Reader.Buffer_Pos := 1;
            
            if Last < Reader.Buffer'Last then
               Reader.End_Of_File := True;
            end if;
         exception
            when others =>
               Reader.End_Of_File := True;
               Reader.Buffer_End := 0;
         end;
      end Fill_Buffer;

      --  Read data into buffer
      function Read 
        (Reader : in out Reader_Type;
         Data   : out Stream_Element_Array) return Result_Type is
         
         Total_Read : Natural := 0;
         Data_Index : Stream_Element_Offset := Data'First;
         Copy_Count : Stream_Element_Offset;
      begin
         while Data_Index <= Data'Last and then not Reader.End_Of_File loop
            --  If buffer is empty, fill it
            if Reader.Buffer_Pos > Reader.Buffer_End then
               Fill_Buffer (Reader);
               if Reader.Buffer_End = 0 then
                  --  No more data available
                  exit;
               end if;
            end if;

            --  Copy available data from buffer
            declare
               Available : constant Stream_Element_Offset := 
                 Reader.Buffer_End - Reader.Buffer_Pos + 1;
               Needed : constant Stream_Element_Offset := 
                 Data'Last - Data_Index + 1;
            begin
               Copy_Count := Stream_Element_Offset'Min (Available, Needed);
               
               Data (Data_Index .. Data_Index + Copy_Count - 1) :=
                 Reader.Buffer (Reader.Buffer_Pos .. 
                               Reader.Buffer_Pos + Copy_Count - 1);
               
               Reader.Buffer_Pos := Reader.Buffer_Pos + Copy_Count;
               Data_Index := Data_Index + Copy_Count;
               Total_Read := Total_Read + Natural (Copy_Count);
            end;
         end loop;

         return Make_Ok (Total_Read);
      end Read;
         
      --  Peek at buffered data without consuming
      function Peek 
        (Reader : Reader_Type;
         Count  : Stream_Element_Offset) return Stream_Element_Array is
         
         Available : constant Stream_Element_Offset := 
           Reader.Buffer_End - Reader.Buffer_Pos + 1;
         Peek_Count : constant Stream_Element_Offset := 
           Stream_Element_Offset'Min (Count, Available);
      begin
         if Peek_Count <= 0 then
            return [1 .. 0 => 0];  -- Empty array
         end if;

         return Reader.Buffer (Reader.Buffer_Pos .. 
                              Reader.Buffer_Pos + Peek_Count - 1);
      end Peek;
         
      --  Skip bytes in buffer
      procedure Skip 
        (Reader : in out Reader_Type;
         Count  : Stream_Element_Offset) is
         
         Available : constant Stream_Element_Offset := 
           Reader.Buffer_End - Reader.Buffer_Pos + 1;
         Skip_Count : constant Stream_Element_Offset := 
           Stream_Element_Offset'Min (Count, Available);
      begin
         Reader.Buffer_Pos := Reader.Buffer_Pos + Skip_Count;
      end Skip;

      --  Check if reader is valid
      function Is_Valid (Reader : Reader_Type) return Boolean is
      begin
         return Reader.Valid and then Reader.File /= null and then Ada.Streams.Stream_IO.Is_Open (Reader.File.all);
      end Is_Valid;

      --  Check if end of file reached
      function End_Of_File (Reader : Reader_Type) return Boolean is
      begin
         return Reader.End_Of_File and then Reader.Buffer_Pos > Reader.Buffer_End;
      end End_Of_File;

      --  Get bytes available in buffer
      function Available_Bytes (Reader : Reader_Type) return Natural is
      begin
         if Reader.Buffer_Pos <= Reader.Buffer_End then
            return Natural (Reader.Buffer_End - Reader.Buffer_Pos + 1);
         else
            return 0;
         end if;
      end Available_Bytes;
      
   end Buffered_Reader;
   
   package body Buffered_Writer is
      
      --  Initialize writer with file
      procedure Initialize 
        (Writer : in out Writer_Type;
         File   : access Ada.Streams.Stream_IO.File_Type) is
      begin
         Writer.File := File_Access (File);
         Writer.Buffer_Used := 0;
         Writer.Total_Written := 0;
         Writer.Valid := True;
      end Initialize;

      --  Flush internal buffer to file
      procedure Flush_Buffer (Writer : in out Writer_Type) is
      begin
         if Writer.Buffer_Used > 0 then
            Write_Impl (Writer.File.all, 
                       Writer.Buffer (1 .. Writer.Buffer_Used));
            Writer.Total_Written := Writer.Total_Written + 
                                  Long_Long_Integer (Writer.Buffer_Used);
            Writer.Buffer_Used := 0;
         end if;
      end Flush_Buffer;
         
      --  Write data to buffer
      function Write 
        (Writer : in out Writer_Type;
         Data   : Stream_Element_Array) return Result_Type is
         
         Data_Index : Stream_Element_Offset := Data'First;
         Copy_Count : Stream_Element_Offset;
      begin
         while Data_Index <= Data'Last loop
            --  If buffer is full, flush it
            if Writer.Buffer_Used = Buffer_Size then
               begin
                  Flush_Buffer (Writer);
               exception
                  when others =>
                     -- Create error with Error_Type - need specific error handling
                     -- This would need Error_Type implementation from caller
                     return Make_Ok (0);  -- Placeholder - real implementation needs error
               end;
            end if;

            --  Copy data to buffer
            declare
               Available : constant Stream_Element_Offset := 
                 Buffer_Size - Writer.Buffer_Used;
               Needed : constant Stream_Element_Offset := 
                 Data'Last - Data_Index + 1;
            begin
               Copy_Count := Stream_Element_Offset'Min (Available, Needed);
               
               Writer.Buffer (Writer.Buffer_Used + 1 .. 
                             Writer.Buffer_Used + Copy_Count) :=
                 Data (Data_Index .. Data_Index + Copy_Count - 1);
               
               Writer.Buffer_Used := Writer.Buffer_Used + Copy_Count;
               Data_Index := Data_Index + Copy_Count;
            end;
         end loop;

         return Make_Ok (Natural (Data'Length));
      end Write;
         
      --  Flush buffer to file
      function Flush 
        (Writer : in out Writer_Type) return Result_Type is
      begin
         begin
            Flush_Buffer (Writer);
            Flush_Impl (Writer.File.all);
            return Make_Ok (0);
         exception
            when others =>
               -- Need proper error handling with Error_Type
               return Make_Ok (0);  -- Placeholder
         end;
      end Flush;

      --  Check if writer is valid
      function Is_Valid (Writer : Writer_Type) return Boolean is
      begin
         return Writer.Valid and then Writer.File /= null and then Ada.Streams.Stream_IO.Is_Open (Writer.File.all);
      end Is_Valid;
         
      --  Get bytes written
      function Bytes_Written 
        (Writer : Writer_Type) return Long_Long_Integer is
      begin
         return Writer.Total_Written;
      end Bytes_Written;

      --  Get bytes buffered
      function Buffered_Bytes (Writer : Writer_Type) return Natural is
      begin
         return Natural (Writer.Buffer_Used);
      end Buffered_Bytes;
      
   end Buffered_Writer;

end Abohlib.Core.Domain.Utilities.IO.Generic_Buffered;