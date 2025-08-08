--  =============================================================================
--  Abohlib.Core.Domain.Services.Pipeline.Builder - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Services.Pipeline.Builder is

   --  ==========================================================================
   --  Pipeline Implementation
   --  ==========================================================================
   
   overriding
   procedure Add_Stage
     (P     : in out Pipeline;
      Stage : access Stage_Interface'Class)
   is
   begin
      P.Stages.Append (Stage_Reference (Stage));
   end Add_Stage;
   
   overriding
   function Process
     (P     : in out Pipeline;
      Input : Input_Type) return Pipeline_Result.Result
   is
      Current_Input  : constant Input_Type := Input;
      Current_Output : Output_Type;
   begin
      -- Process through each stage in sequence
      for Stage of P.Stages loop
         declare
            Result : constant Stage_Result.Result := 
              Stage.Process (Current_Input);
         begin
            if Stage_Result.Is_Ok (Result) then
               Current_Output := Stage_Result.Get_Ok (Result);
               
               -- For intermediate stages, output becomes next input
               -- This assumes Input_Type and Output_Type are compatible
               -- In practice, you'd need type conversions or a more
               -- sophisticated pipeline design
               if Stage /= P.Stages.Last_Element then
                  -- This is a simplification - real implementation would
                  -- handle type conversions between stages
                  null;
               end if;
            else
               -- Return error from failed stage
               return Pipeline_Result.Err (Stage_Result.Get_Err (Result));
            end if;
         end;
      end loop;
      
      return Pipeline_Result.Ok (Current_Output);
   end Process;
   
   overriding
   function Stage_Count (P : Pipeline) return Natural is
   begin
      return Natural (P.Stages.Length);
   end Stage_Count;
   
   overriding
   procedure Reset (P : in out Pipeline) is
   begin
      for Stage of P.Stages loop
         Stage.Reset;
      end loop;
   end Reset;
   
   function Name (P : Pipeline) return String is
   begin
      return To_String (P.Name);
   end Name;
   
   procedure Set_Name (P : in out Pipeline; Name : String) is
   begin
      P.Name := To_Unbounded_String (Name);
   end Set_Name;
   
   --  ==========================================================================
   --  Builder Implementation
   --  ==========================================================================
   
   function Create_Builder (Name : String := "Pipeline") return Pipeline_Builder is
   begin
      return Pipeline_Builder'
        (Name          => To_Unbounded_String (Name),
         Stages        => Stage_Vectors.Empty_Vector,
         Error_Handler => null,
         Built         => False);
   end Create_Builder;
   
   function Add_Stage
     (Builder : in out Pipeline_Builder;
      Stage   : access Stage_Interface'Class) 
      return Pipeline_Builder'Class
   is
   begin
      if Builder.Built then
         raise Program_Error with "Cannot modify builder after Build";
      end if;
      
      Builder.Stages.Append (Stage_Reference (Stage));
      return Builder;
   end Add_Stage;
   
   function Build
     (Builder : in out Pipeline_Builder) 
      return access Pipeline_Interface'Class
   is
      Result : constant Pipeline_Access := new Pipeline;
   begin
      if Builder.Built then
         raise Program_Error with "Builder already used";
      end if;
      
      Result.Name := Builder.Name;
      Result.Stages := Builder.Stages;
      Builder.Built := True;
      
      return Result;
   end Build;
   
   procedure Reset (Builder : in out Pipeline_Builder) is
   begin
      Builder.Stages.Clear;
      Builder.Built := False;
      Builder.Error_Handler := null;
   end Reset;
   
   function With_Name 
     (Builder : in out Pipeline_Builder; 
      Name : String) return Pipeline_Builder'Class
   is
   begin
      if Builder.Built then
         raise Program_Error with "Cannot modify builder after Build";
      end if;
      
      Builder.Name := To_Unbounded_String (Name);
      return Builder;
   end With_Name;
   
   function With_Error_Handler
     (Builder : in out Pipeline_Builder;
      Handler : Error_Handler_Access) 
      return Pipeline_Builder'Class
   is
   begin
      if Builder.Built then
         raise Program_Error with "Cannot modify builder after Build";
      end if;
      
      Builder.Error_Handler := Handler;
      return Builder;
   end With_Error_Handler;

end Abohlib.Core.Domain.Services.Pipeline.Builder;