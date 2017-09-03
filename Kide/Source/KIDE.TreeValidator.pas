{*******************************************************************}
{                                                                   }
{   Kide2 Editor: GUI for Kitto2                                    }
{                                                                   }
{   Copyright (c) 2012-2017 Ethea S.r.l.                            }
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
unit KIDE.TreeValidator;

interface

uses
  SysUtils,
  EF.Tree,
  KIDE.Process;

type
  TTreeValidator = class(TProcess)
  strict private
  protected
    procedure ValidateTree(const ANode: TEFTree);
  end;

implementation

uses
  {$IFDEF DEBUG}Dialogs, System.UITypes,{$ENDIF}
  EF.YAML,
  KIDE.Utils;

{ TTreeValidator }

procedure TTreeValidator.ValidateTree(const ANode: TEFTree);
var
  I, J: Integer;
  LNode, LConfigNode: TEFNode;
  LFileName: string;
  LMetadataConfig: TEFPersistentTree;
  LNodeName: string;
  LNodeValue: string;
  LSameValue: Boolean;
  LTree: TEFTree;
begin
  //Check popupmenu template file
  if ANode.ChildCount > 0 then
  begin
    LFileName := GetMetadataNodeFileName(ANode);
    if FileExists(LFileName) then
    begin
      //Syntax check for subnode
      LMetadataConfig := TEFPersistentTree.Create;
      TEFYAMLReader.LoadTree(LMetadataConfig, LFileName);
      for I := 0 to LMetadataConfig.ChildCount -1 do
      begin
        //Check required nodes
        LConfigNode := LMetadataConfig.Children[I];
        LNodeName := LConfigNode.Name;
        if LConfigNode.GetBoolean('Required') then
        begin
          if not Assigned(ANode.FindNode(LNodeName)) then
            LogError(Format('Missing Node "%s"', [LNodeName]));
        end;
      end;
      for I := 0 to ANode.ChildCount -1 do
      begin
        //Check if node exists in metadata
        LNode := ANode.Children[I];
        LNodeName := LNode.Name;
        LConfigNode := LMetadataConfig.FindNode(LNodeName);
        if not Assigned(LConfigNode) then
        begin
          if Copy(LNodeName,1,1)='.' then
          begin
            LogWarning(Format('The node "%s" is probably disabled', [LNode.GetPath]));
          end
          else
          begin
            LSameValue := False;
            for J := 0 to LMetadataConfig.ChildCount -1 do
            begin
              LConfigNode := LMetadataConfig.Children[J];
              //Check only the Value of node if CheckValue: True
              if LConfigNode.GetBoolean('CheckValue') then
              begin
                LNodeValue := LConfigNode.AsString;
                if SameText(LNodeValue, Copy(LNode.AsString, 1, Length(LNodeValue))) then
                begin
                  LSameValue := True;
                  break;
                end;
              end;
            end;
            {$IFDEF DEBUG}
            if not LSameValue then
            begin
              LogInfo(Format('Node "%s" is probably an application custom node of %s',
                [LNode.GetPath, ExtractFileName(LFileName)]));
            end;
            {$ENDIF}
          end;
        end;
        ValidateTree(LNode);
      end;
    end
    else
    begin
      {$IFDEF DEBUG}
      if MessageDlg(Format('Missing Metadata Template file "%s": do you want to create it?',
        [ExtractFilename(LFileName)]),
        mtWarning, [mbYes, mbNo], 0) = mrYes then
        begin
          LTree := TEFTree.Create;
          try
            TEFYAMLWriter.SaveTree(LTree, LFileName);
          finally
            LTree.Free;
          end;
        end;
      {$ENDIF}
    end;
  end;
end;

end.
