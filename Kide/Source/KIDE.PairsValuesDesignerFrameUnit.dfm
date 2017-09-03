inherited PairsValuesDesignerFrame: TPairsValuesDesignerFrame
  inherited ClientPanel: TPanel
    inherited DesignPanel: TPanel
      ExplicitHeight = 0
      inline PairsValuesFrame: TPairsValuesFrame
        Left = 0
        Top = 22
        Width = 373
        Height = 417
        HelpType = htKeyword
        HelpKeyword = 'BasicConcepts#The_config_file'
        Align = alClient
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        ExplicitTop = 22
        ExplicitWidth = 373
        ExplicitHeight = 417
        inherited ClientPanel: TPanel
          Width = 373
          Height = 395
          ExplicitWidth = 373
          ExplicitHeight = 395
        end
        inherited ToolBar: TToolBar
          Width = 373
          ExplicitWidth = 373
        end
        inherited PairsValueListEditor: TValueListEditor
          Width = 373
          Height = 395
          ExplicitWidth = 373
          ExplicitHeight = 395
          ColWidths = (
            150
            217)
        end
      end
    end
  end
end
