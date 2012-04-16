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
unit EF.YAMLTest;

interface

uses
  TestFramework,
  EF.YAML;

type
  TEFYAMLTestCase = class(TTestCase, ITest)
  private
    FReader: TEFYAMLReader;
    FWriter: TEFYAMLWriter;
    FDeleteSavedFile: Boolean;
  private
    const FILENAME = 'Test.yaml';
    const SAVED_FILENAME = 'Test_saved.yaml';
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    procedure Fail(msg: string; ErrorAddrs: Pointer = nil); override;
  published
    procedure TestRead;
    procedure TestReadAnnotations;
    procedure TestReadWrite;
    procedure TestReadCloneWrite;
  end;

implementation

uses
  Windows, SysUtils,
  EF.Tree, EF.StrUtils,
  Kitto.TestCommon;

{ TEFYAMLTestCase }

procedure TEFYAMLTestCase.Fail(msg: string; ErrorAddrs: Pointer);
begin
  FDeleteSavedFile := False;
  inherited;
end;

procedure TEFYAMLTestCase.SetUp;
begin
  inherited;
  FReader := TEFYAMLReader.Create;
  FWriter := TEFYAMLWriter.Create;
  FDeleteSavedFile := True;
end;

procedure TEFYAMLTestCase.TearDown;
begin
  inherited;
  if FDeleteSavedFile and FileExists(GetWorkDirectory + SAVED_FILENAME) then
    DeleteFile(GetWorkDirectory + SAVED_FILENAME);
  FreeAndNil(FReader);
  FreeAndNil(FWriter);
end;

procedure TEFYAMLTestCase.TestRead;
var
  LTree: TEFTree;
begin
  LTree := TEFTree.Create;
  try
    FReader.LoadTreeFromFile(LTree, GetWorkDirectory + FILENAME);
    CheckEquals(3, LTree.ChildCount);
    CheckEquals(4, LTree.Children[0].ChildCount);
    CheckEqualsString('>', LTree.Children[2].ValueAttributes);
  finally
    FreeAndNil(LTree);
  end;
end;

procedure TEFYAMLTestCase.TestReadAnnotations;
var
  LTree: TEFTree;
begin
  LTree := TEFTree.Create;
  try
    FReader.LoadTreeFromFile(LTree, GetWorkDirectory + FILENAME);
    CheckEquals(2, LTree.Children[0].Children[1].AnnotationCount);
    CheckEquals('# Here comes the second subnode.', LTree.Children[0].Children[1].Annotations[1]);
  finally
    FreeAndNil(LTree);
  end;
end;

procedure TEFYAMLTestCase.TestReadCloneWrite;
var
  LTree1, LTree2: TEFTree;
  LOrig: string;
  LSaved: string;
begin
  LTree1 := TEFTree.Create;
  try
    FReader.LoadTreeFromFile(LTree1, GetWorkDirectory + FILENAME);
    LTree2 := TEFTree.Clone(LTree1);
    try
      FWriter.SaveTreeToFile(LTree2, GetWorkDirectory + SAVED_FILENAME);
    finally
      FreeAndNil(LTree2);
    end;
    LOrig := TextFileToString(GetWorkDirectory + FILENAME);
    LSaved := TextFileToString(GetWorkDirectory + SAVED_FILENAME);
    CheckEqualsString(LOrig, LSaved);
  finally
    FreeAndNil(LTree1);
  end;
end;

procedure TEFYAMLTestCase.TestReadWrite;
var
  LTree: TEFTree;
  LOrig: string;
  LSaved: string;
begin
  LTree := TEFTree.Create;
  try
    FReader.LoadTreeFromFile(LTree, GetWorkDirectory + FILENAME);
    FWriter.SaveTreeToFile(LTree, GetWorkDirectory + SAVED_FILENAME);

    LOrig := TextFileToString(GetWorkDirectory + FILENAME);
    LSaved := TextFileToString(GetWorkDirectory + SAVED_FILENAME);
    CheckEqualsString(LOrig, LSaved);
  finally
    FreeAndNil(LTree);
  end;
end;

initialization
  TestFramework.RegisterTest(TEFYAMLTestCase.Suite);

end.
