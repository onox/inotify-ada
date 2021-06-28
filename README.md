[![Build status](https://github.com/onox/inotify-ada/actions/workflows/build.yaml/badge.svg)](https://github.com/onox/inotify-ada/actions/workflows/build.yaml)
[![Alire crate](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/inotify.json)](https://alire.ada.dev/crates/inotify.html)
[![License](https://img.shields.io/github/license/onox/inotify-ada.svg?color=blue)](https://github.com/onox/inotify-ada/blob/master/LICENSE)
[![GitHub release](https://img.shields.io/github/release/onox/inotify-ada.svg)](https://github.com/onox/inotify-ada/releases/latest)
[![IRC](https://img.shields.io/badge/IRC-%23ada%20on%20libera.chat-orange.svg)](https://libera.chat)
[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.svg)](https://gitter.im/ada-lang/Lobby)

# inotify-ada

An Ada 2012 library to monitor filesystem events using Linux' inotify API.

## Usage

```ada
with Ada.Command_Line;
with Ada.Text_IO;

with Inotify.Recursive;

procedure Example is
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
begin
   Instance.Add_Watch
     (Path => Ada.Command_Line.Argument (1),
      Mask => (Modified | Closed_Write | Closed_No_Write => True, others => False));

   while Instance.Has_Watches loop
      Instance.Process_Events (Handle_Event'Access);
   end loop;
end Example;
```

An optional second access-to-procedure parameter can be added to `Process_Events`
to handle move events. See [`examples/monitor.adb`][url-example].

## Dependencies

In order to build the library, you need to have:

 * An Ada 2012 compiler implementing `GNAT.OS_Lib`

 * [Alire][url-alire] and (optionally) `make`

## Installing dependencies on Ubuntu 18.04 LTS

Install the dependencies using apt:

```sh
$ sudo apt install gnat-7 gprbuild make
```

and then install Alire.

## Using the library

Use the library in your crates as follows:

```
alr with inotify
```

## Installing the tools

A tool to monitor a folder for any event can be build and run with:

```
$ alr run --args="path/to/folder"
```

Alternatively, it can be build and installed with:

```
$ make
$ make PREFIX=~/.local install
```

Run `inotify-ada path/to/folder` to monitor the folder for events.

## Contributing

Please read the [contributing guidelines][url-contributing] before opening
issues or pull requests.

## License

This library is distributed under the terms of the [Apache License 2.0][url-apache].

  [url-alire]: https://alire.ada.dev/
  [url-apache]: https://opensource.org/licenses/Apache-2.0
  [url-contributing]: /CONTRIBUTING.md
  [url-example]: /examples/monitor.adb
