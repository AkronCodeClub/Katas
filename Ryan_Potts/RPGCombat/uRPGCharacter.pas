unit uRPGCharacter;

interface

type
  IRPGCharacter = interface(IInvokable)
    ['{DDA12494-66E5-46C8-BB75-98A35619F08D}']
    function GetHealth: integer;
    function GetLevel: integer;
    function GetAlive: Boolean;
    function GetName: string;
    procedure SetHealth(aValue: integer);
    procedure SetLevel(aValue: integer);
    procedure SetAlive(aValue: Boolean);
    procedure SetDamage(aValue: integer);
    procedure SetHeal(aValue: integer);
    procedure SetTarget(aValue: IRPGCharacter);
    property Name: string read GetName;
    property Health: integer read GetHealth write SetHealth;
    property Level: integer read GetLevel write SetLevel;
    property Alive: Boolean read GetAlive write SetAlive;
    property Damage: integer write SetDamage;
    property Heal: integer write SetHeal;
    property Target: IRPGCharacter write SetTarget;
  end;

  TRPGCharacter = class(TInterfacedObject, IRPGCharacter)
  private
    fTarget: IRPGCharacter;
    fName: string;
    fHealth: integer;
    fLevel: integer;
    fAlive: Boolean;
    function GetHealth: integer;
    function GetLevel: integer;
    function GetAlive: Boolean;
    function GetName: string;
    procedure SetHealth(aValue: integer);
    procedure SetLevel(aValue: integer);
    procedure SetAlive(aValue: Boolean);
    procedure SetDamage(aValue: integer);
    procedure SetHeal(aValue: integer);
    procedure SetTarget(aValue: IRPGCharacter);
  public
    constructor Create(aName: string = '');
    destructor Destroy; override;
    property Name: string read GetName;
    property Health: integer read GetHealth write SetHealth;
    property Level: integer read GetLevel write SetLevel;
    property Alive: Boolean read GetAlive write SetAlive;
    property Damage: integer write SetDamage;
    property Heal: integer write SetHeal;
    property Target: IRPGCharacter write SetTarget;
  end;

implementation

{ TRPGCharacter }

constructor TRPGCharacter.Create(aName: string = '');
begin
  fHealth := 1000;
  fLevel := 1;
  fAlive := true;
  fName := aName;
end;

destructor TRPGCharacter.Destroy;
begin

  inherited;
end;

function TRPGCharacter.GetAlive: Boolean;
begin
  result := fAlive;
end;

function TRPGCharacter.GetHealth: integer;
begin
  result := fHealth;
end;

function TRPGCharacter.GetLevel: integer;
begin
  result := fLevel;
end;

function TRPGCharacter.GetName: string;
begin
  result := fName;
end;

procedure TRPGCharacter.SetAlive(aValue: Boolean);
begin
  fAlive := aValue;
end;

procedure TRPGCharacter.SetDamage(aValue: integer);
var
  targetHealth: integer;
  TotalDamage: integer;
begin
  if (fTarget.Name <> fName) and fTarget.Alive then
  begin
    TotalDamage := aValue;
    if fTarget.Level - fLevel >= 5 then
      TotalDamage := TotalDamage div 2
    else
    if fLevel - fTarget.Level >= 5 then
      TotalDamage := TotalDamage * 2;
    targetHealth := fTarget.Health;
    dec(targetHealth, TotalDamage);
    fTarget.Health := targetHealth;
  end;
end;

procedure TRPGCharacter.SetHeal(aValue: integer);
begin
  if (fTarget = nil) and fAlive then
    SetHealth(fHealth + aValue);
end;

procedure TRPGCharacter.SetHealth(aValue: integer);
begin
  fHealth := aValue;
  if aValue < 0 then
  begin
    fHealth := 0;
    fAlive := false;
  end
  else
  if fHealth > 1000 then
    fHealth := 1000;
end;

procedure TRPGCharacter.SetLevel(aValue: integer);
begin
  fLevel := aValue;
end;

procedure TRPGCharacter.SetTarget(aValue: IRPGCharacter);
begin
  fTarget := aValue;
end;

end.
