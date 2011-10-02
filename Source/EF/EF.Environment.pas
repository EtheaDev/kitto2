unit EF.Environment;

interface

uses
  EF.Macros;

type
  ///	<summary>
  ///	  Interface for the environment object used to expand macros and perform
  ///	  other application-level tasks.
  ///	</summary>
  IEFEnvironment = interface
    ['{EF783642-330E-41F0-93BB-265DDA22BCA1}']
    function GetMacroExpansionEngine: TEFMacroExpansionEngine;

    ///	<summary>
    ///	  This should be used in place of EF's default expansion engine in EF
    ///	  applications, because it is thread-safe when necessary. EF-specific
    ///	  macro expanders should be added here at run time. This engine should
    ///	  be chained to the default engine, so that all default EF macros are
    ///	  supported.
    ///	</summary>
    property MacroExpansionEngine: TEFMacroExpansionEngine read GetMacroExpansionEngine;
  end;

function Environment: IEFEnvironment;

type
  TEFEnvironmentGetFunction = function: IEFEnvironment;

procedure SetEnvironmentGetFunction(const AFunction: TEFEnvironmentGetFunction);

implementation

var
  _ApplicationEnvironment: TEFEnvironmentGetFunction;

function Environment: IEFEnvironment;
begin
  Result := _ApplicationEnvironment;

  Assert(Assigned(Result));
end;

procedure SetEnvironmentGetFunction(const AFunction: TEFEnvironmentGetFunction);
begin
  _ApplicationEnvironment := AFunction;
end;

end.
