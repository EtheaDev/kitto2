unit Ext.Ux.Form;

interface

uses
  StrUtils
  , Ext.Base
  , Ext.Form
  ;

type
  TExtUxFormFileUploadField = class(TExtFormTextField)
  protected
    function GetObjectNamePrefix: string; override;
  public
    class function JSClassName: string; override;
  end;

implementation

uses
  EF.Localization;

class function TExtUxFormFileUploadField.JSClassName: string;
begin
  Result := 'Ext.ux.form.FileUploadField';
end;

function TExtUxFormFileUploadField.GetObjectNamePrefix: string;
begin
  Result := 'uplfld';
end;

end.
