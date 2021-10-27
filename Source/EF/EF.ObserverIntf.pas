{-------------------------------------------------------------------------------
   Copyright 2012 Ethea S.r.l.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-------------------------------------------------------------------------------}

///	<summary>
///	  <para>
///	    This unit defines the interfaces and support classes that implement a
///	    particular EF-specific flavour of the GoF Observer pattern.
///	  </para>
///	  <para>
///	    This flavour introduces context strings and is designed to handle
///	    interfaces that disable reference counting, as all EF interfaces do.
///	  </para>
///	</summary>
unit EF.ObserverIntf;

{$I EF.Defines.inc}

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
    procedure NotifyObserversOnBehalfOf(const ASubject: IEFSubject; const AContext: string = '');
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
begin
  NotifyObserversOnBehalfOf(Self, AContext);
end;

procedure TEFSubjectAndObserver.NotifyObserversOnBehalfOf(
  const ASubject: IEFSubject; const AContext: string);
var
  I: Integer;
  LIntf: IEFObserver;
begin
  if Assigned(FObservers) then
    for I := FObservers.Count - 1 downto 0 do
      if Supports(TObject(FObservers[I]), IEFObserver, LIntf) then
        LIntf.UpdateObserver(ASubject, AContext);
end;

procedure TEFSubjectAndObserver.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
end;

end.
