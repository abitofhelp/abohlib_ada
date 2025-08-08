--  =============================================================================
--  Abohlib.Core.Application.Use_Cases.Simple_Validator - Implementation
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Application.Use_Cases.Simple_Validator is

   function Validate_Length
     (Self       : Simple_Validator;
      Input      : String;
      Min_Length : String_Length_Type := 1;
      Max_Length : String_Length_Type := Default_Max_String_Length) return Validation_Result.Result
   is
      pragma Unreferenced (Self);
   begin
      if Input'Length < Natural(Min_Length) then
         return
           Validation_Result.Err (To_Unbounded_String ("Input too short"));
      elsif Input'Length > Natural(Max_Length) then
         return Validation_Result.Err (To_Unbounded_String ("Input too long"));
      else
         return Validation_Result.Ok (True);
      end if;
   end Validate_Length;

   function Validate_Alphanumeric
     (Self : Simple_Validator; Input : String) return Validation_Result.Result
   is
      pragma Unreferenced (Self);
   begin
      for C of Input loop
         if not (C in 'A' .. 'Z' or C in 'a' .. 'z' or C in '0' .. '9') then
            return
              Validation_Result.Err
                (To_Unbounded_String
                   ("Input contains non-alphanumeric characters"));
         end if;
      end loop;

      return Validation_Result.Ok (True);
   end Validate_Alphanumeric;

   function Sanitize_String
     (Self : Simple_Validator; Input : String) return String_Result.Result
   is
      pragma Unreferenced (Self);
      Result : String (Input'Range);
      Index  : Natural := Result'First - 1;
   begin
      for C of Input loop
         if C in 'A' .. 'Z' or C in 'a' .. 'z' or C in '0' .. '9' or C = ' '
         then
            Index := Index + 1;
            if Index <= Result'Last then
               Result (Index) := C;
            end if;
         end if;
      end loop;

      if Index >= Result'First then
         return
           String_Result.Ok
             (To_Unbounded_String (Result (Result'First .. Index)));
      else
         return String_Result.Ok (To_Unbounded_String (""));
      end if;
   end Sanitize_String;

   function Validate_Batch
     (Self : Simple_Validator; Inputs : String_Array)
      return Validation_Result.Result is
   begin
      for Input of Inputs loop
         declare
            Result : constant Validation_Result.Result :=
              Self.Validate_Length (To_String (Input));
         begin
            if Result.Is_Err then
               return Result;
            end if;
         end;
      end loop;

      return Validation_Result.Ok (True);
   end Validate_Batch;

   function Is_Safe_String
     (Self : Simple_Validator; Input : String) return Boolean
   is
      Result : constant Validation_Result.Result :=
        Self.Validate_Alphanumeric (Input);
   begin
      return Result.Is_Ok;
   end Is_Safe_String;

end Abohlib.Core.Application.Use_Cases.Simple_Validator;
