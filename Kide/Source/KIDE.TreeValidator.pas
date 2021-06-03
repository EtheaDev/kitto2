{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

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
