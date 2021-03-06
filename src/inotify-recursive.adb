--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

with Ada.Containers.Bounded_Vectors;
with Ada.Directories;
with Ada.IO_Exceptions;

package body Inotify.Recursive is

   function "+" (Value : String) return SU.Unbounded_String renames SU.To_Unbounded_String;

   function "+" (Value : SU.Unbounded_String) return String renames SU.To_String;

   package Watch_Vectors is new Ada.Containers.Bounded_Vectors (Positive, Watch);
   package Move_Vectors  is new Ada.Containers.Bounded_Vectors (Positive, Move);

   overriding
   function Add_Watch
     (Object : in out Recursive_Instance;
      Path   :        String;
      Mask   :        Watch_Bits := All_Events) return Watch
   is
      Recursive_Mask : Watch_Bits := Mask;

      procedure Add_Entry (Next_Entry : Ada.Directories.Directory_Entry_Type) is
         use all type Ada.Directories.File_Kind;
         Name : constant String := Ada.Directories.Simple_Name (Next_Entry);
      begin
         if Ada.Directories.Kind (Next_Entry) = Directory and Name not in "." | ".." then
            Object.Add_Watch (Ada.Directories.Compose (Path, Name), Recursive_Mask);
         end if;
      exception
         --  Ignore the folder if the user has no permission to scan it
         --  or if the file is a symlink
         when Ada.IO_Exceptions.Use_Error =>
            null;
      end Add_Entry;
   begin
      Recursive_Mask.Created      := True;
      Recursive_Mask.Deleted_Self := True;
      Recursive_Mask.Moved_From := True;
      Recursive_Mask.Moved_To   := True;
      Recursive_Mask.Moved_Self := True;

      --  Do not follow symlinks
      if Ada.Directories.Full_Name (Path) /= Path then
         raise Ada.IO_Exceptions.Use_Error;
      end if;

      Ada.Directories.Search (Path, "", Process => Add_Entry'Access);
      return Result : constant Watch := Instance (Object).Add_Watch (Path, Recursive_Mask) do
         Object.Masks.Insert (Result.Watch, Mask);
      end return;
   end Add_Watch;

   procedure Remove_Children (Object : in out Recursive_Instance; Subject : Watch) is
      Path : constant String := Object.Watches.Element (Subject.Watch);

      Watches : Watch_Vectors.Vector (Capacity => Object.Watches.Length);

      procedure Iterate (Position : Watch_Maps.Cursor) is
         Other_Path : constant String := Watch_Maps.Element (Position);
      begin
         if Other_Path'Length > Path'Length
           and then Path & '/' = Other_Path (1 .. Path'Length + 1)
         then
            Watches.Append ((Watch => Watch_Maps.Key (Position)));
         end if;
      end Iterate;
   begin
      Object.Watches.Iterate (Iterate'Access);
      for Element of Watches loop
         Instance (Object).Remove_Watch (Element);
         Object.Masks.Delete (Element.Watch);
      end loop;
   end Remove_Children;

   overriding
   procedure Remove_Watch (Object : in out Recursive_Instance; Subject : Watch) is
   begin
      --  Procedure Process_Events might read multiple events for a specific
      --  watch and the callback for the first event may immediately try to
      --  remove the watch
      if Object.Defer_Remove then
         if not Object.Pending_Removals.Contains (Subject) then
            Object.Pending_Removals.Append (Subject);
         end if;
         return;
      end if;

      Object.Remove_Children (Subject);
      Instance (Object).Remove_Watch (Subject);
      Object.Masks.Delete (Subject.Watch);
   end Remove_Watch;

   overriding
   procedure Process_Events
     (Object : in out Recursive_Instance;
      Handle :        not null access procedure
        (Subject      : Watch;
         Event        : Event_Kind;
         Is_Directory : Boolean;
         Name         : String);
      Move_Handle : not null access procedure
        (Subject      : Watch;
         Is_Directory : Boolean;
         From, To     : String))
   is
      Moves : Move_Vectors.Vector (Capacity => Object.Watches.Length);

      procedure Handle_Event
        (Subject      : Inotify.Watch;
         Event        : Inotify.Event_Kind;
         Is_Directory : Boolean;
         Name         : String)
      is
         Mask : constant Watch_Bits := Object.Masks (Subject.Watch);
      begin
         case Event is
            when Created =>
               if Mask.Created then
                  Handle (Subject, Event, Is_Directory, Name);
               end if;

               if Is_Directory then
                  Object.Add_Watch (Name, Mask);
               end if;
            when Deleted_Self =>
               if Mask.Deleted_Self then
                  Handle (Subject, Event, Is_Directory, Name);
                  --  TODO Is_Directory is always False even if inode is a directory
               end if;

               --  The OS will already have deleted the watch and generated
               --  an Ignored event, which caused the watch to be deleted from
               --  Object.Watches in Instance.Process_Events
               Object.Masks.Delete (Subject.Watch);
            when Moved_From =>
               if Mask.Moved_From then
                  Handle (Subject, Event, Is_Directory, Name);
               end if;
            when Moved_To =>
               if Mask.Moved_To then
                  Handle (Subject, Event, Is_Directory, Name);
               end if;
            when Moved_Self =>
               if Mask.Moved_Self then
                  Handle (Subject, Event, Is_Directory, Name);
                  --  TODO Is_Directory is always False even if inode is a directory
               end if;

               declare
                  Cursor : Move_Vectors.Cursor := Move_Vectors.No_Element;

                  procedure Process_Move (Position : Move_Vectors.Cursor) is
                     Element : constant Move := Moves (Position);
                  begin
                     if +Element.From = Name then
                        Object.Remove_Watch (Subject);
                        Object.Add_Watch (+Element.To, Mask);
                        Cursor := Position;
                     end if;
                  end Process_Move;

                  use type Move_Vectors.Cursor;
               begin
                  Moves.Iterate (Process_Move'Access);
                  if Cursor /= Move_Vectors.No_Element then
                     Moves.Delete (Cursor);
                  else
                     Object.Remove_Watch (Subject);
                     --  TODO Delete cookie as well
                  end if;
               end;
            when others =>
               Handle (Subject, Event, Is_Directory, Name);
         end case;
      end Handle_Event;

      procedure Handle_Move_Event
        (Subject      : Watch;
         Is_Directory : Boolean;
         From, To     : String) is
      begin
         Move_Handle (Subject, Is_Directory, From, To);

         if Is_Directory then
            if From /= "" then
               Moves.Append ((+From, +To));
            else
               Object.Add_Watch (To, Object.Masks.Element (Subject.Watch));
            end if;
         end if;
      end Handle_Move_Event;
   begin
      Instance (Object).Process_Events (Handle_Event'Access, Handle_Move_Event'Access);
   end Process_Events;

   overriding
   procedure Process_Events
     (Object : in out Recursive_Instance;
      Handle :        not null access procedure
        (Subject      : Watch;
         Event        : Event_Kind;
         Is_Directory : Boolean;
         Name         : String))
   is
      procedure Move_Handle
        (Subject      : Watch;
         Is_Directory : Boolean;
         From, To     : String) is null;
   begin
      Object.Process_Events (Handle, Move_Handle'Access);
   end Process_Events;

end Inotify.Recursive;
