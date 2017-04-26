# Cayan Delphi Components

Delphi Library for Payment Integration using Cayan Solutions

## Disclaimer

This library was written by Jerry Dodge, and not by any direct member of Cayan. No guarantees are made towards this code, and is available on an as-is basis. Neither I or Cayan are responsible for any problems which arise as a result of using this code.

Please visit www.cayan.com for more information.

## Disclaimer

While this library is free and open-source, you are still responsible to acquire certification with Cayan to be allowed to work with their solutions. This library only wraps Cayan's APIs, and you must treat the integration the same as if you wrote this library. Certification works on a per-application basis, so even one certification is not enough to use this library indefinitely.

Please visit https://cayan.com/developers for more information.

## Super Object

This library uses its own copy of X-SuperObject. Since there were modifications in those units to be able to work as needed, it was necessary to include them in this repo. However, it must be noted that the units `Cayan.XSuperObject` and `Cayan.XSuperJSON` were not actually written with the rest of this library.

## Delphi Support

This library was written for Delphi 10 Seattle. While it should work in most modern Delphi versions, it has not been tested, and there is no guarantee that it will be compatible.

## Delphi Packages

There are two packages included: A Run-Time package, and a Design-Time package. Each must be installed into the Delphi IDE the same as any standard package.

1. Open the Run-Time Package `CayanComponentsDX10.dpk` and `Build` the project.
2. Open the Design-Time Package `dclCayanComponentsDX10.dpk` and `Build` the project.
3. While still in the Design-Time Package, `Install` the project.
4. You shall be notified that the components were successfully installed.
5. Open `Tools` > `Options`, then navigate to `Environment Options` > `Delphi Options` > `Library`, choose the appropriate `Selected Platform`, then click on the `[...]` button to the right of `Library Path`. 
⋅⋅1. Enter the directory of the `.\Source` dir and press `Add`. 
--2. Enter the directory of the `.\Libraries\X-SuperObject` dir and press `Add`.
--3. Press `OK`, and `OK`.



