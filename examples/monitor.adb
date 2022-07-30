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

with Ada.Command_Line;
with Ada.Text_IO;

with Inotify.Recursive;

procedure Monitor is
   Instance : Inotify.Recursive.Recursive_Instance;

   procedure Handle_Event
     (Subject      : Inotify.Watch;
      Event        : Inotify.Event_Kind;
      Is_Directory : Boolean;
      Name         : String)
   is
      Kind : constant String := (if Is_Directory then "directory" else "file");
   begin
      Ada.Text_IO.Put_Line (Event'Image & " " & Instance.Name (Subject));
      Ada.Text_IO.Put_Line ("  [" & Kind & "] '" & Name & "'");
   end Handle_Event;

   procedure Handle_Move_Event
     (Unused_Subject : Inotify.Watch;
      Is_Directory   : Boolean;
      From, To       : String)
   is
      Kind : constant String := (if Is_Directory then "directory" else "file");
   begin
      if From /= "" then
         Ada.Text_IO.Put_Line ("moved " & Kind & " '" & From & "' to '" & To & "'");
      else
         Ada.Text_IO.Put_Line ("moved new " & Kind & " to '" & To & "'");
      end if;
   end Handle_Move_Event;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: monitor <path>");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   Instance.Add_Watch (Path => Ada.Command_Line.Argument (1));
   while Instance.Has_Watches loop
      Instance.Process_Events (Handle_Event'Access, Handle_Move_Event'Access);
   end loop;
end Monitor;
