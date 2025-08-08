--  =============================================================================
--  Abohlib.Infrastructure.Testing.Property_Testing - Implementation
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Fixed;
with Abohlib.Core.Domain.Utilities.ULID_Helpers;

package body Abohlib.Infrastructure.Testing.Property_Testing is

   --  ==========================================================================
   --  Random Manager Implementation
   --  ==========================================================================

   protected body Random_Manager is

      procedure Initialize (Seed : Natural) is
      begin
         Random_Natural.Reset (State.Natural_Gen, Seed);
         Random_Integer.Reset (State.Integer_Gen, Seed);
         State.Initialized := True;
      end Initialize;

      procedure Get_Natural
        (Max : Natural := Natural'Last; Result : out Natural) is
      begin
         if not State.Initialized then
            Random_Natural.Reset (State.Natural_Gen, 42);
            Random_Integer.Reset (State.Integer_Gen, 42);
            State.Initialized := True;
         end if;

         if Max = Natural'Last then
            Result := Random_Natural.Random (State.Natural_Gen);
         else
            Result := Random_Natural.Random (State.Natural_Gen) mod (Max + 1);
         end if;
      end Get_Natural;

      procedure Get_Integer (Min, Max : Integer; Result : out Integer) is
         Range_Size : constant Natural := Natural (Max - Min);
         Temp       : Natural;
      begin
         if not State.Initialized then
            Random_Natural.Reset (State.Natural_Gen, 42);
            Random_Integer.Reset (State.Integer_Gen, 42);
            State.Initialized := True;
         end if;

         Get_Natural (Range_Size, Temp);
         Result := Min + Integer (Temp);
      end Get_Integer;

      procedure Get_Boolean (Result : out Boolean) is
         Temp : Natural;
      begin
         Get_Natural (1, Temp);
         Result := Temp = 1;
      end Get_Boolean;

   end Random_Manager;

   --  ==========================================================================
   --  Property Framework Implementation
   --  ==========================================================================

   package body Property_Framework is

      function Check_Property
        (Property : Property_Function; Config : Test_Config := Default_Config)
         return Property_Result.Result
      is
         Failures : Natural := 0;
         pragma Unreferenced (Failures);
      begin
         Random_Manager.Initialize (Natural(Config.Seed));

         for Test_Number in 1 .. Natural(Config.Max_Tests) loop
            declare
               Test_Data : constant Test_Data_Type := Generate;
            begin
               if not Property (Test_Data) then
                  --  Property failed, try to shrink
                  declare
                     Current_Data : Test_Data_Type := Test_Data;
                     Shrink_Count : Natural := 0;
                  begin
                     while Shrink_Count < Natural(Config.Max_Shrinks) loop
                        declare
                           Shrunk_Data : constant Test_Data_Type :=
                             Shrink (Current_Data);
                        begin
                           if not Property (Shrunk_Data) then
                              Current_Data := Shrunk_Data;
                              Shrink_Count := Shrink_Count + 1;
                           else
                              exit;
                           end if;
                        end;
                     end loop;

                     return
                       Property_Result.Err
                         ((Kind         => Property_Violation,
                           Message      =>
                             To_Unbounded_String ("Property failed"),
                           Test_Number  => Test_Number,
                           Shrink_Steps => Shrink_Count,
                           Example_Data =>
                             To_Unbounded_String (To_String (Current_Data))));
                  end;
               end if;
            end;
         end loop;

         return Property_Result.Ok (True);
      end Check_Property;

      function Check_Properties
        (Properties : Property_Array; Config : Test_Config := Default_Config)
         return Property_Result.Result is
      begin
         for Property of Properties loop
            declare
               Result : constant Property_Result.Result :=
                 Check_Property (Property, Config);
            begin
               if Result.Is_Err then
                  return Result;
               end if;
            end;
         end loop;

         return Property_Result.Ok (True);
      end Check_Properties;

   end Property_Framework;

   --  ==========================================================================
   --  Built-in Generators Implementation
   --  ==========================================================================

   package body Integer_Generator is

      function Generate_Integer (Min, Max : Integer) return Integer is
         Result : Integer;
      begin
         Random_Manager.Get_Integer (Min, Max, Result);
         return Result;
      end Generate_Integer;

      function Shrink_Integer (Value, Target : Integer) return Integer is
      begin
         if Value > Target then
            return Value - (Value - Target) / 2;
         elsif Value < Target then
            return Value + (Target - Value) / 2;
         else
            return Value;
         end if;
      end Shrink_Integer;

   end Integer_Generator;

   package body Natural_Generator is

      function Generate_Natural (Max : Natural := Natural'Last) return Natural
      is
         Result : Natural;
      begin
         Random_Manager.Get_Natural (Max, Result);
         return Result;
      end Generate_Natural;

      function Shrink_Natural (Value : Natural) return Natural is
      begin
         return Value / 2;
      end Shrink_Natural;

   end Natural_Generator;

   package body String_Generator is

      function Generate_String (Min_Length, Max_Length : Natural) return String
      is
         Length_Range : Natural;
         Length       : Natural;
         --  Result removed as it's not used
         Char_Val     : Natural;
      begin
         Random_Manager.Get_Natural (Max_Length - Min_Length, Length_Range);
         Length := Length_Range + Min_Length;

         if Length = 0 then
            return "";
         end if;

         declare
            Final_Result : String (1 .. Length);
         begin
            for I in Final_Result'Range loop
               Random_Manager.Get_Natural (95, Char_Val);
               Final_Result (I) := Character'Val (Char_Val + 32);
            end loop;
            return Final_Result;
         end;
      end Generate_String;

      function Shrink_String (Value : String) return String is
      begin
         if Value'Length <= 1 then
            return Value;
         else
            return Value (Value'First .. Value'First + Value'Length / 2 - 1);
         end if;
      end Shrink_String;

   end String_Generator;

   package body Boolean_Generator is

      function Generate_Boolean return Boolean is
         Result : Boolean;
      begin
         Random_Manager.Get_Boolean (Result);
         return Result;
      end Generate_Boolean;

      function Shrink_Boolean (Value : Boolean) return Boolean is
         pragma Unreferenced (Value);
      begin
         return False;  --  Shrink towards False
      end Shrink_Boolean;

   end Boolean_Generator;

   --  ==========================================================================
   --  Domain-Specific Properties Implementation
   --  ==========================================================================

   package body ULID_Properties is

      function Generate_Valid_ULID_String return String is
         --  Generate a valid ULID string (26 characters)
         Valid_Chars : constant String := "0123456789ABCDEFGHJKMNPQRSTVWXYZ";
         Result      : String (1 .. Abohlib.Core.Domain.Utilities.ULID_Helpers.ULID_String_Length);
         Char_Index  : Natural;
      begin
         for I in Result'Range loop
            Random_Manager.Get_Natural (Valid_Chars'Length - 1, Char_Index);
            Result (I) := Valid_Chars (Char_Index + 1);
         end loop;
         return Result;
      end Generate_Valid_ULID_String;

      function Generate_Invalid_ULID_String return String is
         Invalid_Cases : constant array (1 .. 5) of String (1 .. 10) :=
           ["          ",         --  Empty (padded to 10 chars)
            "SHORT     ",    --  Too short
            "TOOLONGFOR", --  Too long (truncated to 10)
            "INVALID!! ",--  Invalid characters
            "lowercase "];
         Case_Index    : Natural;
      begin
         Random_Manager.Get_Natural (4, Case_Index);
         return
           Ada.Strings.Fixed.Trim
             (Invalid_Cases (Case_Index + 1), Ada.Strings.Both);
      end Generate_Invalid_ULID_String;

      function ULID_Ordering_Property (First_ULID : String) return Boolean is
         Second_ULID : constant String := Generate_Valid_ULID_String;
      begin
         --  Simple lexicographic ordering check
         return First_ULID < Second_ULID or First_ULID = Second_ULID;
      end ULID_Ordering_Property;

      function ULID_Validation_Property (ULID_String : String) return Boolean
      is
      begin
         --  Basic validation: length and character set
         if ULID_String'Length /= Abohlib.Core.Domain.Utilities.ULID_Helpers.ULID_String_Length then
            return False;
         end if;

         for C of ULID_String loop
            if C not in '0' .. '9' and C not in 'A' .. 'Z' then
               return False;
            end if;
         end loop;

         return True;
      end ULID_Validation_Property;

   end ULID_Properties;

   package body File_Path_Properties is

      function Generate_Valid_Path return String is
         Path_Parts : constant array (1 .. 3) of String (1 .. 10) :=
           ["/home/user", "/tmp/files", "/var/local"];
         Part_Index : Natural;
      begin
         Random_Manager.Get_Natural (2, Part_Index);
         return Path_Parts (Part_Index + 1);
      end Generate_Valid_Path;

      function Generate_Invalid_Path return String is
         Invalid_Paths : constant array (1 .. 3) of Unbounded_String :=
           [To_Unbounded_String ("../../../etc/passwd"),
            To_Unbounded_String ("..\\windows\\system32"),
            To_Unbounded_String ("$(rm -rf /)")];
         Path_Index    : Natural;
      begin
         Random_Manager.Get_Natural (2, Path_Index);
         return To_String (Invalid_Paths (Path_Index + 1));
      end Generate_Invalid_Path;

      function Path_Normalization_Property (Path : String) return Boolean is
      begin
         --  Simplified normalization check
         return not (Ada.Strings.Fixed.Index (Path, "..") > 0);
      end Path_Normalization_Property;

      function Path_Security_Property (Path : String) return Boolean is
      begin
         --  Basic security checks
         return
           not (Ada.Strings.Fixed.Index (Path, "..") > 0
                or Ada.Strings.Fixed.Index (Path, "$(") > 0);
      end Path_Security_Property;

   end File_Path_Properties;

   --  ==========================================================================
   --  Common Properties Implementation
   --  ==========================================================================

   function Commutativity_Property (A, B : T) return Boolean is
   begin
      return Op (A, B) = Op (B, A);
   end Commutativity_Property;

   function Associativity_Property (A, B, C : T) return Boolean is
   begin
      return Op (Op (A, B), C) = Op (A, Op (B, C));
   end Associativity_Property;

   function Identity_Property (A : T) return Boolean is
   begin
      return Op (A, Identity) = A and Op (Identity, A) = A;
   end Identity_Property;

   function Idempotency_Property (A : T) return Boolean is
   begin
      return F (F (A)) = F (A);
   end Idempotency_Property;

end Abohlib.Infrastructure.Testing.Property_Testing;
