unit EF.ObserverIntf;

interface

uses
  Generics.Collections,
  EF.Intf;

type
  IEFSubject = interface;

  ///	<summary>
  ///	  The observer interface. See GoF. This particular implementation allows
  ///	  for one observer to handle many subjects (and have the sender subject
  ///	  passed to the Update method), and manage different kinds of updates
  ///	  based on a context string parameter.
  ///	</summary>
  IEFObserver = interface(IEFInterface)
    ['{72299440-39BC-4EB8-91E8-710961FAC8B2}']
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string = '');
  end;

  ///	<summary>
  ///	  The subject interface. See GoF.
  ///	</summary>
  IEFSubject = interface(IEFInterface)
    ['{1778901C-8D1E-4F2C-8AC2-CD6D0537E45D}']
    procedure AttachObserver(const AObserver: IEFObserver);
    procedure DetachObserver(const AObserver: IEFObserver);
    procedure NotifyObservers(const AContext: string = '');
  end;

  ///	<summary>
  ///	  <para>
  ///	    A default implementation of IEFObserver and IEFSubject. Inherit from
  ///	    this class whenever you need to create an observer or subject and
  ///	    don't have any inheritance constraint.
  ///	  </para>
  ///	  <para>
  ///	    This class can also be embedded in order to delegate implementation
  ///	    to it.
  ///	  </para>
  ///	</summary>
  TEFSubjectAndObserver = class(TEFNoRefCountObject, IEFSubject, IEFObserver)
  private
    // Storing raw pointers prevents the compiler from calling _Release on
    // referenced objects (which might no longer be alive) upon destruction of
    // the list.
    FObservers: TList<Pointer>;
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
  FObservers := TList<Pointer>.Create;
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
    if FObservers.IndexOf(Pointer(AObserver.AsObject)) < 0 then
      FObservers.Add(Pointer(AObserver.AsObject));
  end;
end;

procedure TEFSubjectAndObserver.DetachObserver(
  const AObserver: IEFObserver);
begin
  if Assigned(AObserver) and Assigned(FObservers) then
    FObservers.Remove(Pointer(AObserver.AsObject));
end;

procedure TEFSubjectAndObserver.NotifyObservers(const AContext: string);
var
  I: Integer;
  LIntf: IEFObserver;
begin
  if Assigned(FObservers) then
    for I := FObservers.Count - 1 downto 0 do
      if Supports(TObject(FObservers[I]), IEFObserver, LIntf) then
        LIntf.UpdateObserver(Self, AContext);
end;

procedure TEFSubjectAndObserver.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
end;

end.
