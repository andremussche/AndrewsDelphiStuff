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

The Original Code is Denomo.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit Denomo;

{***********************************************************************
  WARNING:
  Never include this unit in your release version.
  Only include it when you are debugging.
***********************************************************************}

interface

{$include Denomo.inc}

uses
{$ifdef UseDenomoMM}
  DenomoModifiedFastMM4,
{$endif}
  DenomoConfig,
  DenomoMemHook, DenomoUtils;

implementation

initialization
{$ifndef DEBUG}
  if PromptRunInReleaseVerion then
    ErrorOrExit('WARNING: Denomo is compiled and running without debug information.'#13#10
      + 'Please only use Denomo in debug version.'#13#10
      + 'Never use Denomo in your distribution verion.'#13#10
      + #13#10
      + 'To disable this message, set PromptRunInReleaseVerion in DenomoConfig to False',
      'Warning');
{$endif}

  InitLeakMemHook;

finalization
  DeInitLeakMemHook;

end.

