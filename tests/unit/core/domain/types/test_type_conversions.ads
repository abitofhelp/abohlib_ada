--   =============================================================================
--   Test_Type_Conversions - Unit tests for type conversion functions
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Type_Conversions is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   SI_Bytes_Type ↔ Long_Long_Integer tests
   function Test_SI_Bytes_To_Long_Long_Integer
     (Output : access Test_Output_Port'Class) return Boolean;

   function Test_Long_Long_Integer_To_SI_Bytes
     (Output : access Test_Output_Port'Class) return Boolean;

   function Test_Long_Long_Integer_Boundary
     (Output : access Test_Output_Port'Class) return Boolean;

--   Natural ↔ SI_Bytes_Type tests
   function Test_Natural_To_SI_Bytes
     (Output : access Test_Output_Port'Class) return Boolean;

   function Test_SI_Bytes_To_Natural
     (Output : access Test_Output_Port'Class) return Boolean;

   function Test_Natural_Boundary_Values
     (Output : access Test_Output_Port'Class) return Boolean;

--   Duration ↔ Milliseconds_Type tests
   function Test_Duration_To_Milliseconds
     (Output : access Test_Output_Port'Class) return Boolean;

   function Test_Milliseconds_To_Duration
     (Output : access Test_Output_Port'Class) return Boolean;

   function Test_Duration_Precision
     (Output : access Test_Output_Port'Class) return Boolean;

--   Calculate_MB_Per_Second tests
   function Test_Calculate_MB_Per_Second
     (Output : access Test_Output_Port'Class) return Boolean;

   function Test_Calculate_MB_Per_Second_Complex
     (Output : access Test_Output_Port'Class) return Boolean;

--   Calculate_Percentage tests
   function Test_Calculate_Percentage_Basic
     (Output : access Test_Output_Port'Class) return Boolean;

   function Test_Calculate_Percentage_Complex
     (Output : access Test_Output_Port'Class) return Boolean;

   function Test_Calculate_Percentage_Boundaries
     (Output : access Test_Output_Port'Class) return Boolean;

--   Compression ratio test
   function Test_Compression_Ratio
     (Output : access Test_Output_Port'Class) return Boolean;

--   New byte conversion tests
   function Test_SI_IEC_Conversions
     (Output : access Test_Output_Port'Class) return Boolean;

   function Test_From_KB_MB_GB
     (Output : access Test_Output_Port'Class) return Boolean;

   function Test_From_KB_MB_GB_Overflow_Protection
     (Output : access Test_Output_Port'Class) return Boolean;

--   New time conversion tests
   function Test_Milliseconds_Seconds_Conversions
     (Output : access Test_Output_Port'Class) return Boolean;

   function Test_Timeout_Delay_To_Duration
     (Output : access Test_Output_Port'Class) return Boolean;

--   Performance type conversion tests
   function Test_Compression_Ratio_Float_Conversions
     (Output : access Test_Output_Port'Class) return Boolean;

   function Test_Speed_Float_Conversions
     (Output : access Test_Output_Port'Class) return Boolean;

--   Run all tests
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Natural;

end Test_Type_Conversions;