unit KIDE.MainDataModuleUnit;

interface

uses
  System.SysUtils, System.Classes;

type
  TMainDataModule = class(TDataModule)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

end.
