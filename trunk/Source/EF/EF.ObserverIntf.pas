{*******************************************************************}
{                                                                   }
{   Ethea Foundation                                                }
{   Interfaces and base implementation of the                       }
{   observer design pattern.                                        }
{                                                                   }
{   Copyright (c) 2006-2010 Ethea Srl                               }
{   ALL RIGHTS RESERVED / TUTTI I DIRITTI RISERVATI                 }
{                                                                   }
{*******************************************************************}
{                                                                   }
{   The entire contents of this file is protected by                }
{   International Copyright Laws. Unauthorized reproduction,        }
{   reverse-engineering, and distribution of all or any portion of  }
{   the code contained in this file is strictly prohibited and may  }
{   result in severe civil and criminal penalties and will be       }
{   prosecuted to the maximum extent possible under the law.        }
{                                                                   }
{   RESTRICTIONS                                                    }
{                                                                   }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED      }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE        }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE       }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT  }
{   AND PERMISSION FROM ETHEA S.R.L.                                }
{                                                                   }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON       }
{   ADDITIONAL RESTRICTIONS.                                        }
{                                                                   }
{*******************************************************************}
{                                                                   }
{   Il contenuto di questo file è protetto dalle leggi              }
{   internazionali sul Copyright. Sono vietate la riproduzione, il  }
{   reverse-engineering e la distribuzione non autorizzate di tutto }
{   o parte del codice contenuto in questo file. Ogni infrazione    }
{   sarà perseguita civilmente e penalmente a termini di legge.     }
{                                                                   }
{   RESTRIZIONI                                                     }
{                                                                   }
{   SONO VIETATE, SENZA IL CONSENSO SCRITTO DA PARTE DI             }
{   ETHEA S.R.L., LA COPIA, LA VENDITA, LA DISTRIBUZIONE E IL       }
{   TRASFERIMENTO A TERZI, A QUALUNQUE TITOLO, DEL CODICE SORGENTE  }
{   CONTENUTO IN QUESTO FILE E ALTRI FILE AD ESSO COLLEGATI.        }
{                                                                   }
{   SI FACCIA RIFERIMENTO ALLA LICENZA D'USO PER INFORMAZIONI SU    }
{   EVENTUALI RESTRIZIONI ULTERIORI.                                }
{                                                                   }
{*******************************************************************} 

{
  Defines the interfaces used to implement the observer pattern in EF.
}
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
  TEFClassesSubjectAndObserver = class(TEFNoRefCountObject, IEFSubject, IEFObserver)
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
  
{ TEFClassesSubjectAndObserver }

procedure TEFClassesSubjectAndObserver.AfterConstruction;
begin
  inherited;
  FObservers := TInterfaceList.Create;
end;

destructor TEFClassesSubjectAndObserver.Destroy;
begin
  FreeAndNil(FObservers);
  inherited;
end;

procedure TEFClassesSubjectAndObserver.AttachObserver(
  const AObserver: IEFObserver);
begin
  if Assigned(AObserver) and Assigned(FObservers) then
  begin
    if FObservers.IndexOf(AObserver) < 0 then
      FObservers.Add(AObserver);
  end;
end;

procedure TEFClassesSubjectAndObserver.DetachObserver(
  const AObserver: IEFObserver);
begin
  if Assigned(AObserver) and Assigned(FObservers) then
    FObservers.Remove(AObserver);
end;

procedure TEFClassesSubjectAndObserver.NotifyObservers(const AContext: string);
var
  I: Integer;
begin
  if Assigned(FObservers) then
    for I := FObservers.Count - 1 downto 0 do
      (FObservers[I] as IEFObserver).UpdateObserver(Self, AContext);
end;

procedure TEFClassesSubjectAndObserver.UpdateObserver(const ASubject: IEFSubject;
  const AContext: string);
begin
end;

end.
