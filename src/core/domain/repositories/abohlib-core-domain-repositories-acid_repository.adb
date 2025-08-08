--  =============================================================================
--  Abohlib.Core.Domain.Repositories.ACID_Repository - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Repositories.ACID_Repository is

   pragma Unreferenced (Aggregate_Name);
   pragma Unreferenced ("=");
   pragma Unreferenced (Id_To_String);
   pragma Unreferenced (Get_Id);
   pragma Unreferenced (Get_Version);

   --  ==========================================================================
   --  Query Support Implementation
   --  ==========================================================================

   function Find_By_Specification
     (Self : ACID_Repository_Interface'Class) return List_Result.Result
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Matches);
      Error : constant Repository_Error :=
        (Kind             => Invalid_State,
         Message          =>
           To_Unbounded_String ("Find_By_Specification not implemented"),
         Entity_Id        => Null_Unbounded_String,
         Details          =>
           To_Unbounded_String
             ("This method must be overridden in concrete implementations"),
         Expected_Version => 0,
         Actual_Version   => 0);
   begin
      -- This is a default implementation that should be overridden
      -- by concrete repository implementations
      return List_Result.Err (Error);
   end Find_By_Specification;

   --  ==========================================================================
   --  Transaction Helper Implementation
   --  ==========================================================================

   function Execute_In_Transaction
     (Self      : ACID_Repository_Interface'Class;
      Isolation : Isolation_Level := Read_Committed) return Operation_Result
   is
      Transaction_Started : Boolean := False;
      Result              : Operation_Result;
   begin
      -- Start transaction if not already in one
      if not Self.In_Transaction then
         declare
            Begin_Result : constant Transaction_Result.Result :=
              Self.Begin_Transaction (Isolation);
         begin
            if Transaction_Result.Is_Err (Begin_Result) then
               -- Return a default result indicating failure
               -- The actual error handling depends on Operation_Result type
               raise Program_Error with "Failed to begin transaction";
            end if;
            Transaction_Started := True;
         end;
      end if;

      -- Execute the operation
      begin
         Result := Execute (Self);

         -- Commit if we started the transaction and operation succeeded
         if Transaction_Started and then Is_Success (Result) then
            declare
               Commit_Result : constant Transaction_Result.Result :=
                 Self.Commit;
            begin
               if Transaction_Result.Is_Err (Commit_Result) then
                  -- Try to rollback
                  declare
                     Rollback_Result : constant Transaction_Result.Result :=
                       Self.Rollback;
                     pragma Unreferenced (Rollback_Result);
                  begin
                     raise Program_Error with "Failed to commit transaction";
                  end;
               end if;
            end;
         elsif Transaction_Started then
            -- Rollback if we started the transaction and operation failed
            declare
               Rollback_Result : constant Transaction_Result.Result :=
                 Self.Rollback;
               pragma Unreferenced (Rollback_Result);
            begin
               null; -- Ignore rollback errors
            end;
         end if;

         return Result;

      exception
         when others =>
            -- Ensure we rollback on any exception if we started the transaction
            if Transaction_Started and then Self.In_Transaction then
               declare
                  Rollback_Result : constant Transaction_Result.Result :=
                    Self.Rollback;
                  pragma Unreferenced (Rollback_Result);
               begin
                  null; -- Ignore rollback errors
               end;
            end if;
            raise;
      end;
   end Execute_In_Transaction;

end Abohlib.Core.Domain.Repositories.ACID_Repository;
