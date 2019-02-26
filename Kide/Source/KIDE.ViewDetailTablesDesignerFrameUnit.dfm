inherited ViewDetailTablesDesignerFrame: TViewDetailTablesDesignerFrame
  Width = 404
  HelpKeyword = 'RelationshipOneToMany'
  ExplicitWidth = 404
  inherited ClientPanel: TPanel
    Width = 404
    ExplicitWidth = 404
    inherited DesignPanel: TPanel
      Width = 404
      ExplicitWidth = 404
      object DetailTablesPageControl: TPageControl
        Left = 0
        Top = 59
        Width = 404
        Height = 240
        ActivePage = DetailTableTabSheet
        Align = alClient
        TabOrder = 0
        object DetailTableTabSheet: TTabSheet
          Caption = 'DetailTables'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
      end
      object ControllerGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 404
        Height = 59
        Align = alTop
        Caption = 'Controller'
        TabOrder = 1
        object StyleLabel: TLabel
          Left = 8
          Top = 17
          Width = 24
          Height = 13
          Caption = 'Style'
        end
        object StyleHeightLabel: TLabel
          Left = 159
          Top = 17
          Width = 31
          Height = 13
          Caption = 'Height'
        end
        object ControllerStyleComboBox: TComboBox
          Left = 8
          Top = 32
          Width = 145
          Height = 21
          TabOrder = 0
          Text = 'Tabs'
        end
        object _Controller_Style_Height: TSpinEdit
          Left = 159
          Top = 32
          Width = 69
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
      end
    end
  end
  inherited ActionList: TActionList
    object FileOpenAction: TFileOpen
      Category = 'File'
      Dialog.DefaultExt = '*.png'
      Dialog.Filter = 
        'png image files|*.png|gif image files|*.gif|jpeg image files|*.j' +
        'pg;*.jpeg|bitmap image files|*.bmp|All files|*.*'
      Hint = 'Open|Opens an existing file'
      ImageIndex = 0
      ShortCut = 16463
    end
  end
end
