[![License](https://img.shields.io/github/license/onox/inotify-ada.svg?color=blue)](https://github.com/onox/inotify-ada/blob/master/LICENSE)
[![Build status](https://img.shields.io/shippable/5d18ebf6cecb900006e7c241/master.svg)](https://app.shippable.com/github/onox/inotify-ada)
[![GitHub release](https://img.shields.io/github/release/onox/inotify-ada.svg)](https://github.com/onox/inotify-ada/releases/latest)
[![IRC](https://img.shields.io/badge/IRC-%23ada%20on%20freenode-orange.svg)](https://webchat.freenode.net/?channels=ada)

# inotify-ada

An Ada 2012 library for monitoring filesystem events using Linux' inotify API.

## Usage

```ada
with Ada.Command_Line;
with Ada.Text_IO;

with Inotify;

procedure Example is
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
   I.Add_Watch
     (Path => Ada.Command_Line.Argument (1);
      Mask => (Modified | Closed_Write | Closed_No_Write => True, others => False));
   I.Process_Events (Handle_Event'Access);
end Main;
```

## Dependencies

In order to build the library, you need to have:

 * An Ada 2012 compiler implementing `GNAT.OS_Lib`

 * GPRBuild and `make`

## Installing dependencies on Ubuntu 18.04 LTS

Install the dependencies using apt:

```sh
$ sudo apt install gnat-7 gprbuild make
```

## Installation

A Makefile is provided to build the source code. Use `make` to build
the source code:

```
$ make
```

You can override CFLAGS if desired. After having compiled the source code,
the library can be installed by executing:

```
$ make PREFIX=/usr install
```

Change `PREFIX` to the preferred destination folder, for example `~/.local`.
Import `inotify_ada` in your \*.gpr project file:

```ada
with "inotify_ada";
```

## Contributing

Read the [contributing guidelines][url-contributing] if you want to add
a bugfix or an improvement.

## License

This library is distributed under the terms of the [Apache License 2.0][url-apache].

  [url-contributing]: /CONTRIBUTING.md
  [url-apache]: https://opensource.org/licenses/Apache-2.0
