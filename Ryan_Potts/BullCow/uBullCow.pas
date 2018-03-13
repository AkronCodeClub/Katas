unit uBullCow;

interface

type
  TSecretNumber = TArray<integer>;

  TGuess = record
    Bulls: integer;
    Cows: integer;
  end;

  TGuessResult = record
    Success: Boolean;
    SecretNumber: TSecretNumber;
    GuessScore: TGuess;
    Turns: integer;
    errMsg: string;
  end;

  IBullsAndCows = interface(IInvokable)
    ['{3789C9B2-A707-486A-A6BE-8A5CBA0D16CF}']
    function makeAGuess(aGuess: string): TGuessResult;
  end;

function NewBullsAndCows(aSeedNumber: string = 'random'): IBullsAndCows;

implementation
uses Math, SysUtils;
{ TBullsAndCows }

type
  TBullsAndCows = class(TInterfacedObject, IBullsAndCows)
  private
    fSecretNumber: TSecretNumber;
    fTurns: integer;
    fGuess: TSecretNumber;
    fScoredGuess: TGuess;
    function WellFormed(aSecretNumber: TSecretNumber): boolean;
    function MakeSecretNumber: TSecretNumber;
    function ScoreGuess(aSecretnumber: TSecretNumber): TGuess;
    procedure SetGuess(aValue: string);
    function StrToGuess(aValue: string): TSecretNumber;
    function Wins(Guess: TSecretNumber): boolean;
  public
    constructor Create(aSeedNumber: string = 'random');
    function makeAGuess(aGuess: string): TGuessResult;
  end;

function NewBullsAndCows(aSeedNumber: string = 'random'): IBullsAndCows;
begin
  result := TBullsAndCows.Create(aSeedNumber);
end;

constructor TBullsAndCows.Create(aSeedNumber: string = 'random');
begin
  fTurns := 0;
  Randomize;
  SetLength(fGuess, 0);
  if aSeedNumber = 'random' then
  begin
    fSecretNumber := MakeSecretNumber;
    if not WellFormed(fSecretNumber) then
      fSecretNumber := MakeSecretNumber;
  end
  else
    fSecretNumber := StrToGuess(aSeedNumber);
end;

function TBullsAndCows.MakeSecretNumber: TSecretNumber;
var
  i: integer;
begin
  SetLength(result, 4);
  for i := Low(result) to High(result) do
    result[i] := RandomRange(1, 9);
end;

function TBullsAndCows.makeAGuess(aGuess: string): TGuessResult;
begin
  inc(fTurns);
  SetLength(result.SecretNumber, 0);
  fillchar(result, sizeof(result), #0);
  try
    result.Turns := fTurns;
    SetGuess(aGuess);
    if Wins(fGuess) then
    begin
      result.SecretNumber := fSecretNumber;
      result.Success := true;
    end
    else
      result.GuessScore := ScoreGuess(fGuess);
  except
    on E: Exception do
    begin
      result.errMsg := E.Message;
    end;
  end;
end;

function TBullsAndCows.ScoreGuess(aSecretnumber: TSecretNumber): TGuess;
var
  i, j: integer;
begin
  fillchar(result, sizeof(result), #0);
  for i := low(aSecretNumber) to High(aSecretNumber) do
    for j := Low(aSecretNumber) to High(aSecretNumber) do
      if fSecretNumber[i] = aSecretNumber[j] then
      begin
        if i = j then
          inc(result.Bulls)
        else
          inc(result.Cows);
      end;
end;

procedure TBullsAndCows.SetGuess(aValue: string);
begin
  if aValue.Length <> 4 then
    raise Exception.Create('Guess must be a 4 digit number!');

  fGuess := StrToGuess(aValue);
  if not WellFormed(fGuess) then
  begin
    SetLength(fGuess, 0);
    raise Exception.Create('Guess must contain 4 unique digits!');
  end;
end;

function TBullsAndCows.StrToGuess(aValue: string): TSecretNumber;
var
 i: integer;
begin
  SetLength(result, aValue.Length);
  for i := low(aValue) to high(aValue) do
  begin
    result[i-1] := StrToInt(aValue[i]);
  end;
end;

function TBullsAndCows.WellFormed(aSecretNumber: TSecretNumber): boolean;
var
  current, check: integer;
begin
  Result := True;
  for current := 1 to 4 do
    for check := current + 1 to 4 do
      if aSecretNumber[check] = aSecretNumber[current] then
        Result := False;
end;

function TBullsAndCows.Wins(Guess: TSecretNumber): boolean;
var
  i: integer;
begin
  if length(Guess) = length(fSecretNumber) then
  begin
    result := true;
    for i := Low(Guess) to High(Guess) do
      if Guess[i] <> fSecretNumber[i] then
        exit(false);
  end
  else
    result := false;
end;

end.
