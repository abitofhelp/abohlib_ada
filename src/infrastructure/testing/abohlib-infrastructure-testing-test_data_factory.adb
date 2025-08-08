--  =============================================================================
--  Abohlib.Infrastructure.Testing.Test_Data_Factory - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Unchecked_Deallocation;
with Abohlib.Core.Domain.Errors;

package body Abohlib.Infrastructure.Testing.Test_Data_Factory is

   use Test_Data;
   use Abohlib.Core.Domain.Errors;
   
   --  ==========================================================================
   --  Helper Functions
   --  ==========================================================================
   
   procedure Free is new Ada.Unchecked_Deallocation
     (Stream_Element_Array, Stream_Element_Array_Access);
   
   function To_Stream_Element_Array 
     (S : Unbounded_String) return Stream_Element_Array
   is
      Str : constant String := To_String (S);
      Result : Stream_Element_Array (1 .. Str'Length);
   begin
      for I in Str'Range loop
         Result (Stream_Element_Offset (I - Str'First + 1)) := 
           Stream_Element (Character'Pos (Str (I)));
      end loop;
      return Result;
   end To_Stream_Element_Array;

   --  ==========================================================================
   --  Data Builder Implementation
   --  ==========================================================================
   
   function Create_Data_Builder return Test_Data_Builder is
   begin
      return (Size => 0, Pattern => Random, Custom_Pattern => Null_Unbounded_String, Seed => 0);
   end Create_Data_Builder;
   
   function With_Size 
     (Builder : Test_Data_Builder; 
      Size    : Natural) return Test_Data_Builder
   is
   begin
      return (Size => Size, 
              Pattern => Builder.Pattern,
              Custom_Pattern => Builder.Custom_Pattern,
              Seed => Builder.Seed);
   end With_Size;
   
   function With_Predefined_Size
     (Builder : Test_Data_Builder;
      Size    : Natural) return Test_Data_Builder
   is
   begin
      return With_Size (Builder, Size);
   end With_Predefined_Size;
   
   function With_Pattern
     (Builder : Test_Data_Builder;
      Pattern : Pattern_Type) return Test_Data_Builder
   is
   begin
      return (Size => Builder.Size,
              Pattern => Pattern,
              Custom_Pattern => Builder.Custom_Pattern,
              Seed => Builder.Seed);
   end With_Pattern;
   
   function With_Custom_Pattern
     (Builder : Test_Data_Builder;
      Pattern : Stream_Element_Array) return Test_Data_Builder
   is
      Custom_String : Unbounded_String;
   begin
      for E of Pattern loop
         Append (Custom_String, Character'Val (Natural (E)));
      end loop;
      
      return (Size => Builder.Size,
              Pattern => Custom,
              Custom_Pattern => Custom_String,
              Seed => Builder.Seed);
   end With_Custom_Pattern;
   
   function With_Seed
     (Builder : Test_Data_Builder;
      Seed    : Natural) return Test_Data_Builder
   is
   begin
      return (Size => Builder.Size,
              Pattern => Builder.Pattern,
              Custom_Pattern => Builder.Custom_Pattern,
              Seed => Seed);
   end With_Seed;
   
   function Build (Builder : Test_Data_Builder) return Data_Result.Result
   is
      Buffer : Stream_Element_Array_Access;
   begin
      --  Allocate buffer
      Buffer := new Stream_Element_Array (1 .. Stream_Element_Offset (Builder.Size));
      
      --  Fill based on pattern
      case Builder.Pattern is
         when All_Zeros =>
            Fill_Zero (Buffer.all);
            
         when All_Ones =>
            for I in Buffer.all'Range loop
               Buffer (I) := 16#FF#;
            end loop;
            
         when Alternating_Bits =>
            for I in Buffer.all'Range loop
               if Integer (I) mod 2 = 0 then
                  Buffer (I) := 16#AA#;
               else
                  Buffer (I) := 16#55#;
               end if;
            end loop;
            
         when Sequential =>
            Fill_Sequential (Buffer.all);
            
         when Random =>
            Fill_Random (Buffer.all, Builder.Seed);
            
         when Custom =>
            if Length (Builder.Custom_Pattern) > 0 then
               declare
                  Pattern : constant Stream_Element_Array := 
                    To_Stream_Element_Array (Builder.Custom_Pattern);
               begin
                  Fill_Pattern (Buffer.all, Pattern);
               end;
            else
               --  Default to zeros if no custom pattern
               Fill_Zero (Buffer.all);
            end if;
      end case;
      
      return Data_Result.Ok (Buffer);
      
   exception
      when Storage_Error =>
         if Buffer /= null then
            Free (Buffer);
         end if;
         declare
            Error : Factory_Error;
         begin
            Error.Base := Abohlib.Core.Domain.Errors.Default_Domain_Error;
            Error.Base.Category := Abohlib.Core.Domain.Errors.Resource_Error;
            Error.Base.Message := Abohlib.Core.Domain.Errors.Error_Strings.To_Bounded_String
              ("Failed to allocate " & Builder.Size'Image & " bytes");
            return Data_Result.Err (Error);
         end;
   end Build;
   
   function Get_Size (Builder : Test_Data_Builder) return Natural is
     (Builder.Size);
   
   function Get_Pattern (Builder : Test_Data_Builder) return Pattern_Type is
     (Builder.Pattern);
   
   function Get_Seed (Builder : Test_Data_Builder) return Natural is
     (Builder.Seed);

   --  ==========================================================================
   --  File Builder Implementation
   --  ==========================================================================
   
   function Create_File_Builder return Test_File_Builder is
   begin
      return (Path => Null_Unbounded_String,
              Size => 0,
              Pattern => Random,
              Seed => 0);
   end Create_File_Builder;
   
   function With_Path
     (Builder : Test_File_Builder;
      Path    : String) return Test_File_Builder
   is
   begin
      return (Path => To_Unbounded_String (Path),
              Size => Builder.Size,
              Pattern => Builder.Pattern,
              Seed => Builder.Seed);
   end With_Path;
   
   function With_Unique_Name
     (Builder : Test_File_Builder;
      Prefix  : String := "test_";
      Suffix  : String := ".dat") return Test_File_Builder
   is
   begin
      return With_Path (Builder, Unique_Test_Filename (Prefix, Suffix));
   end With_Unique_Name;
   
   function With_Size
     (Builder : Test_File_Builder;
      Size    : Natural) return Test_File_Builder
   is
   begin
      return (Path => Builder.Path,
              Size => Size,
              Pattern => Builder.Pattern,
              Seed => Builder.Seed);
   end With_Size;
   
   function With_Predefined_Size
     (Builder : Test_File_Builder;
      Size    : Natural) return Test_File_Builder
   is
   begin
      return With_Size (Builder, Size);
   end With_Predefined_Size;
   
   function With_Content
     (Builder : Test_File_Builder;
      Pattern : Pattern_Type) return Test_File_Builder
   is
   begin
      return (Path => Builder.Path,
              Size => Builder.Size,
              Pattern => Pattern,
              Seed => Builder.Seed);
   end With_Content;
   
   function With_Seed
     (Builder : Test_File_Builder;
      Seed    : Natural) return Test_File_Builder
   is
   begin
      return (Path => Builder.Path,
              Size => Builder.Size,
              Pattern => Builder.Pattern,
              Seed => Seed);
   end With_Seed;
   
   function Build (Builder : Test_File_Builder) return File_Result.Result
   is
      Path : constant String := To_String (Builder.Path);
   begin
      case Builder.Pattern is
         when All_Zeros =>
            return Generate_Zero_File (Path, Builder.Size);
            
         when Random =>
            return Generate_Random_File (Path, Builder.Size, Builder.Seed);
            
         when others =>
            --  For other patterns, generate data first then write
            declare
               Data_Builder : constant Test_Data_Builder :=
                 Create_Data_Builder
                   .With_Size (Builder.Size)
                   .With_Pattern (Builder.Pattern)
                   .With_Seed (Builder.Seed);
               Build_Result : constant Data_Result.Result := Data_Builder.Build;
            begin
               if Build_Result.Is_Ok then
                  declare
                     Buffer : Stream_Element_Array_Access := Build_Result.Get_Ok;
                     Result : constant File_Result.Result := Generate_Random_File (Path, Builder.Size, Builder.Seed);
                  begin
                     --  Write buffer to file using existing Test_Data functions
                     --  For now, use Generate_Random_File as a placeholder
                     Free (Buffer);
                     return Result;
                  end;
               else
                  --  Convert error
                  declare
                     Error : Factory_Error;
                  begin
                     Error := Build_Result.Get_Err;
                     return File_Result.Err (Error);
                  end;
               end if;
            end;
      end case;
   end Build;
   
   function Get_Path (Builder : Test_File_Builder) return String is
     (To_String (Builder.Path));
   
   function Get_Size (Builder : Test_File_Builder) return Natural is
     (Builder.Size);
   
   function Get_Pattern (Builder : Test_File_Builder) return Pattern_Type is
     (Builder.Pattern);
   
   function Get_Seed (Builder : Test_File_Builder) return Natural is
     (Builder.Seed);

   --  ==========================================================================
   --  Convenience Factory Functions
   --  ==========================================================================
   
   function Create_Zero_Data (Size : Natural) return Data_Result.Result is
   begin
      return Create_Data_Builder
        .With_Size (Size)
        .With_Pattern (All_Zeros)
        .Build;
   end Create_Zero_Data;
   
   function Create_Random_Data 
     (Size : Natural; 
      Seed : Natural := 0) return Data_Result.Result
   is
   begin
      return Create_Data_Builder
        .With_Size (Size)
        .With_Pattern (Random)
        .With_Seed (Seed)
        .Build;
   end Create_Random_Data;
   
   function Create_Sequential_Data (Size : Natural) return Data_Result.Result is
   begin
      return Create_Data_Builder
        .With_Size (Size)
        .With_Pattern (Sequential)
        .Build;
   end Create_Sequential_Data;
   
   function Create_Pattern_Data
     (Size    : Natural;
      Pattern : Stream_Element_Array) return Data_Result.Result
   is
   begin
      return Create_Data_Builder
        .With_Size (Size)
        .With_Custom_Pattern (Pattern)
        .Build;
   end Create_Pattern_Data;
   
   function Create_Temp_Test_File
     (Size    : Natural;
      Pattern : Pattern_Type := Random) return File_Result.Result
   is
   begin
      return Create_File_Builder
        .With_Unique_Name
        .With_Size (Size)
        .With_Content (Pattern)
        .Build;
   end Create_Temp_Test_File;
   
   function Create_Named_Test_File
     (Path    : String;
      Size    : Natural;
      Pattern : Pattern_Type := Random) return File_Result.Result
   is
   begin
      return Create_File_Builder
        .With_Path (Path)
        .With_Size (Size)
        .With_Content (Pattern)
        .Build;
   end Create_Named_Test_File;

end Abohlib.Infrastructure.Testing.Test_Data_Factory;