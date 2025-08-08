--  =============================================================================
--  Abohlib.Infrastructure.Testing.Test_Data - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Streams.Stream_IO;
with Ada.Calendar;
with Abohlib.Core.Domain.Errors; use Abohlib.Core.Domain.Errors;

package body Abohlib.Infrastructure.Testing.Test_Data is

   --  Simple counter for unique names
   Unique_Counter : Natural := 0;

   --  ==========================================================================
   --  Error Helpers
   --  ==========================================================================
   
   function Make_File_IO_Error (Path : String; Message : String) return Test_Data_Error is
   begin
      return Test_Data_Error'(
         Base => Make_Resource_Error(
            Kind => Unavailable,
            Resource_Type => "File",
            Message => "File I/O error for '" & Path & "': " & Message,
            Recovery => "Check file permissions and disk space"
         ).Base
      );
   end Make_File_IO_Error;

   --  ==========================================================================
   --  Data Patterns
   --  ==========================================================================

   procedure Fill_Zero
     (Buffer : out Stream_Element_Array) is
   begin
      Buffer := [others => 0];
   end Fill_Zero;

   procedure Fill_Sequential
     (Buffer : out Stream_Element_Array;
      Start  : Stream_Element := 0) is
   begin
      for I in Buffer'Range loop
         Buffer (I) := Stream_Element ((Natural (Start) + Natural (I - Buffer'First)) mod Byte_Modulus);
      end loop;
   end Fill_Sequential;

   procedure Fill_Pattern
     (Buffer  : out Stream_Element_Array;
      Pattern : Stream_Element_Array) is
      Pattern_Index : Stream_Element_Offset := Pattern'First;
   begin
      for I in Buffer'Range loop
         Buffer (I) := Pattern (Pattern_Index);
         if Pattern_Index = Pattern'Last then
            Pattern_Index := Pattern'First;
         else
            Pattern_Index := Pattern_Index + 1;
         end if;
      end loop;
   end Fill_Pattern;

   procedure Fill_Random
     (Buffer : out Stream_Element_Array;
      Seed   : Natural := 0) is
      -- Simple Linear Congruential Generator
      Current : Long_Long_Integer := 
         (if Seed = 0 then Long_Long_Integer (Ada.Calendar.Seconds (Ada.Calendar.Clock)) else Long_Long_Integer (Seed));
   begin
      for I in Buffer'Range loop
         Current := (Current * LCG_Multiplier + LCG_Increment) mod LCG_Modulus;
         Buffer (I) := Stream_Element (Natural (Current mod Byte_Modulus));
      end loop;
   end Fill_Random;

   --  ==========================================================================
   --  File Generation
   --  ==========================================================================

   function Generate_Zero_File
     (Path : String;
      Size : Natural) return Void_Result.Result is
      use Ada.Streams.Stream_IO;
      File : File_Type;
      Buffer : Stream_Element_Array (1 .. Stream_Element_Offset'Min (Default_Chunk_Size, Stream_Element_Offset (Size)));
   begin
      begin
         Create (File, Out_File, Path);
      exception
         when others =>
            return Void_Result.Err (Make_File_IO_Error (Path, "Failed to create file"));
      end;
      
      Fill_Zero (Buffer);
      
      declare
         Remaining : Natural := Size;
      begin
         while Remaining > 0 loop
            declare
               To_Write : constant Natural := Natural'Min (Remaining, Buffer'Length);
               Chunk : Stream_Element_Array (1 .. Stream_Element_Offset (To_Write));
            begin
               Chunk := Buffer (1 .. Stream_Element_Offset (To_Write));
               begin
                  Write (File, Chunk);
               exception
                  when others =>
                     Close (File);
                     return Void_Result.Err (Make_File_IO_Error (Path, "Failed to write data"));
               end;
               Remaining := Remaining - To_Write;
            end;
         end loop;
      end;
      
      begin
         Close (File);
      exception
         when others =>
            return Void_Result.Err (Make_File_IO_Error (Path, "Failed to close file"));
      end;
      
      return Void_Result.Ok (True);
   end Generate_Zero_File;

   function Generate_Random_File
     (Path : String;
      Size : Natural;
      Seed : Natural := 0) return Void_Result.Result is
      use Ada.Streams.Stream_IO;
      File : File_Type;
      Current_Seed : Natural := Seed;
   begin
      begin
         Create (File, Out_File, Path);
      exception
         when others =>
            return Void_Result.Err (Make_File_IO_Error (Path, "Failed to create file"));
      end;
      
      declare
         Remaining : Natural := Size;
         Buffer_Size : constant := Default_Chunk_Size;
      begin
         while Remaining > 0 loop
            declare
               To_Write : constant Natural := Natural'Min (Remaining, Buffer_Size);
               Chunk : Stream_Element_Array (1 .. Stream_Element_Offset (To_Write));
            begin
               Fill_Random (Chunk, Current_Seed);
               begin
                  Write (File, Chunk);
               exception
                  when others =>
                     Close (File);
                     return Void_Result.Err (Make_File_IO_Error (Path, "Failed to write data"));
               end;
               Remaining := Remaining - To_Write;
               Current_Seed := Current_Seed + 1; -- Vary seed for each chunk
            end;
         end loop;
      end;
      
      begin
         Close (File);
      exception
         when others =>
            return Void_Result.Err (Make_File_IO_Error (Path, "Failed to close file"));
      end;
      
      return Void_Result.Ok (True);
   end Generate_Random_File;

   --  ==========================================================================
   --  Utilities
   --  ==========================================================================

   function Unique_Test_Filename
     (Prefix    : String := "test_";
      Extension : String := ".dat") return String is
   begin
      Unique_Counter := Unique_Counter + 1;
      return Prefix & Unique_Counter'Image (2 .. Unique_Counter'Image'Last) & Extension;
   end Unique_Test_Filename;

   function Cleanup_Test_Files
     (Directory : String;
      Pattern   : String := "test_*.dat") return Void_Result.Result is
      use Ada.Directories;
      Search : Search_Type;
      Dir_Entry : Directory_Entry_Type;
      Files_Deleted : Natural := 0;
   begin
      if not Exists (Directory) then
         return Void_Result.Ok (True); -- Directory doesn't exist, nothing to clean
      end if;
      
      begin
         Start_Search (Search, Directory, Pattern);
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Dir_Entry);
            if Kind (Dir_Entry) = Ordinary_File then
               begin
                  Delete_File (Full_Name (Dir_Entry));
                  Files_Deleted := Files_Deleted + 1;
               exception
                  when others =>
                     End_Search (Search);
                     return Void_Result.Err (Make_File_IO_Error (
                        Full_Name (Dir_Entry), 
                        "Failed to delete file"
                     ));
               end;
            end if;
         end loop;
         End_Search (Search);
      exception
         when others =>
            return Void_Result.Err (Make_File_IO_Error (Directory, "Failed to search directory"));
      end;
      
      return Void_Result.Ok (True);
   end Cleanup_Test_Files;
   
end Abohlib.Infrastructure.Testing.Test_Data;