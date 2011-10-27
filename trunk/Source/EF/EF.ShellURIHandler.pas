{
  Definition of the shell URI handler, which forwards an URI to the Windows
  Shell.
}
unit EF.ShellURIHandler;

interface

uses
  EF.URIProcessor;

  type
  {
    Forwards the URI to the Windows Shell. It is registered at the highest
    possible slot in the URI handler registry so that other handlers have a
    chance to handle an URI before it is forwarded to the OS.
  }
  TEFShellURIHandler = class(TEFURIHandler)
  protected
    procedure InternalHandleURI(var AURI: string; var AHandled: Boolean); override;
  end;

implementation

uses
  Windows, Forms, ShellAPI;

{ TEFShellURIHandler }

procedure TEFShellURIHandler.InternalHandleURI(var AURI: string; var AHandled: Boolean);
begin
  inherited;
  AHandled := ShellExecute(Application.Handle, 'open', PChar(AURI), '', '', SW_SHOW) > 32;
end;

initialization
  URIHandlerRegistry.RegisterHandler(TEFShellURIHandler, MaxInt);

finalization
  URIHandlerRegistry.UnregisterHandler(TEFShellURIHandler);

end.
