{***********************************************************************
Denomo 2.1.0
http://www.kbasm.com/

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
for the specific language governing rights and limitations under the
License.

The Original Code is Denomo.inc

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


{Options include file}

{=====Options define start. For document, search doc-OptionName in this file.}

{**************************************************************************
An option starts with $.define is disabled by default.
To enable it, remove the . (dot).
**************************************************************************}


{.$define UseDenomoMM}

{=====Options define end}


{=====Option tweak section start. Don't modify.}

{=====Option tweak section end}


{=====Options documents start. Don't modify.}

{**************************************************************************
doc-UseDenomoMM
Enable to use the memory manager distributed with Denomo.
Note the memory manager is a modified version of FastMM4.
You should better always enable this option.
This version FastMM can do in-place reallocation which required by Denomo
to detection wrong pointer access.
Denomo requires smaller size block to be resized in place.
**************************************************************************}


{=====Options documents end}

