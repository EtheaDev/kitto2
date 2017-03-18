unit Ext.Draw;

interface

uses
  Ext.Base
  ;

type
  TExtDrawContainer = class(TExtPanel)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtDrawSpriteSprite = class(TExtObject)
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
  end;

  TExtDrawSpriteText = class(TExtDrawSpriteSprite)
  private
    FFontSize: Integer;
    FFontName: string;
    FX: Integer;
    FY: Integer;
    FText: string;
    procedure SetFontName(const AValue: string);
    procedure SetFontSize(const AValue: Integer);
    procedure SetText(const AValue: string);
    procedure SetX(const AValue: Integer);
    procedure SetY(const AValue: Integer);
  public
    class function JSClassName: string; override;
    class function JSXType: string; override;
    property FontName: string read FFontName write SetFontName;
    property FontSize: Integer read FFontSize write SetFontSize;
    property Text: string read FText write SetText;
    property X: Integer read FX write SetX;
    property Y: Integer read FY write SetY;
  end;

implementation

{ TExtDrawContainer }

class function TExtDrawContainer.JSClassName: string;
begin
  Result := 'Ext.draw.Container';
end;

class function TExtDrawContainer.JSXType: string;
begin
  Result := 'draw';
end;

{ TExtDrawSpriteSprite }

class function TExtDrawSpriteSprite.JSClassName: string;
begin
  Result := 'Ext.draw.sprite.Sprite';
end;

class function TExtDrawSpriteSprite.JSXType: string;
begin
  Result := 'sprite.sprite';
end;

{ TExtDrawSpriteText }

class function TExtDrawSpriteText.JSClassName: string;
begin
  Result := 'Ext.draw.sprite.Text';
end;

class function TExtDrawSpriteText.JSXType: string;
begin
  Result := 'sprite.text';
end;

procedure TExtDrawSpriteText.SetFontName(const AValue: string);
begin
  FFontName := SetConfigItem('fontName', 'setFontName', AValue);
end;

procedure TExtDrawSpriteText.SetFontSize(const AValue: Integer);
begin
  FFontSize := SetConfigItem('fontSize', 'setFontSize', AValue);
end;

procedure TExtDrawSpriteText.SetText(const AValue: string);
begin
  FText := SetConfigItem('text', 'setText', AValue);
end;

procedure TExtDrawSpriteText.SetX(const AValue: Integer);
begin
  FX := SetConfigItem('x', 'setX', AValue);
end;

procedure TExtDrawSpriteText.SetY(const AValue: Integer);
begin
  FY := SetConfigItem('y', 'setY', AValue);
end;

end.
