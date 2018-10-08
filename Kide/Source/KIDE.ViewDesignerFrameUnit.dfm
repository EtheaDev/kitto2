inherited ViewDesignerFrame: TViewDesignerFrame
  Width = 497
  Height = 318
  ExplicitWidth = 497
  ExplicitHeight = 318
  inherited ClientPanel: TPanel
    Width = 476
    Height = 318
    ExplicitWidth = 476
    ExplicitHeight = 318
    inherited DesignPanel: TPanel
      Width = 476
      Height = 261
      ExplicitWidth = 476
      ExplicitHeight = 261
      inherited ToolBar: TToolBar
        Width = 476
        ExplicitWidth = 476
      end
    end
    inherited PathPanel: TStaticText
      Width = 476
    end
    inherited CodeEditorFrame: TCodeEditorFrame
      Top = 278
      Width = 476
      ExplicitTop = 278
      ExplicitWidth = 476
      inherited ErrorLabel: TLabel
        Width = 476
        ExplicitTop = 302
        ExplicitWidth = 434
      end
      inherited ToolBar: TToolBar
        Width = 476
        ExplicitWidth = 476
      end
    end
  end
  inherited TabSet: TTabSet
    Left = 476
    Height = 318
    ExplicitLeft = 476
    ExplicitHeight = 318
  end
  inherited ActionList: TActionList
    Top = 184
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
