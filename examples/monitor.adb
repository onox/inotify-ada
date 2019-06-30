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

with Inotify;

procedure Monitor is
   procedure Handle_Event
     (Subject      : Inotify.Watch;
      Event        : Inotify.Event_Kind;
      Is_Directory : Boolean;
      Name         : String) is
   begin
      Ada.Text_IO.Put_Line (Event'Image);

      if Is_Directory then
         Ada.Text_IO.Put_Line ("  [directory] '" & Name & "'");
      else
         Ada.Text_IO.Put_Line ("  [file] '" & Name & "'");
      end if;
   end Handle_Event;

   I : Inotify.Instance;
begin
   I.Add_Watch (Path => Ada.Command_Line.Argument (1));
   I.Process_Events (Handle_Event'Access);
end Monitor;
