unit EF.ObserverIntf;

interface

uses
  Classes,
  EF.Intf;

type
  {
    Forward declaration for IEFSubject.
  }
  IEFSubject = interface;

  {
    The observer interface. See GoF.
    This particular implementation allows for one observer to handle many
    subjects (and have the sender subject passed to the Update method), and
    manage different kinds of updates based on a string parameter.
  }
  IEFObserver = interface(IEFInterface)
    ['{72299440-39BC-4EB8-91E8-710961FAC8B2}']
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = '');
  end;

  {
    The subject interface. See GoF.
  }
  IEFSubject = interface(IEFInterface)
    ['{1778901C-8D1E-4F2C-8AC2-CD6D0537E45D}']
    procedure AttachObserver(const AObserver: IEFObserver);
    procedure DetachObserver(const AObserver: IEFObserver);
    procedure NotifyObservers(const AContext: string = '');
  end;

  {
    A default implementation of IEFObserver and IEFSubject. Inherit from this
    class whenever you need to create an observer or subject and don't have any
    inheritance constraint.

    This class can also be embedded in order to delegate implementation to it.
  }
  TEFSubjectAndObserver = class(TEFNoRefCountObject, IEFSubject, IEFObserver)
  private
    FObservers: TInterfaceList;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = ''); virtual;
    procedure AttachObserver(const AObserver: IEFObserver);
    procedure DetachObserver(const AObserver: IEFObserver);
    procedure NotifyObservers(const AContext: string = '');
  end;

implementation

uses
  SysUtils;
  
{ TEFSubjectAndObserver }

procedure TEFSubjectAndObserver.AfterConstruction;
begin
  inherited;
  FObservers := TInterfaceList.Create;
end;

destructor TEFSubjectAndObserver.Destroy;
begin
  FreeAndNil(FObservers);
  inherited;
end;

procedure TEFSubjectAndObserver.AttachObserver(
  const AObserver: IEFObserver);
begin
  if Assigned(AObserver) and Assigned(FObservers) then
  begin
    if FObservers.IndexOf(AObserver) < 0 then
      FObservers.Add(AObserver);
  end;
end;

procedure TEFSubjectAndObserver.DetachObserver(
  const AObserver: IEFObserver);
begin
  if Assigned(AObserver) and Assigned(FObservers) then
    FObservers.Remove(AObserver);
end;

procedure TEFSubjectAndObserver.NotifyObservers(const AContext: string);
var
  I: Integer;
begin
  if Assigned(FObservers) then
    for I := FObservers.Count - 1 downto 0 do
      (FObservers[I] as IEFObserver).UpdateObserver(Self, AContext);
end;

procedure TEFSubjectAndObserver.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
end;

end.
