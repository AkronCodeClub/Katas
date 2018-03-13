unit uTestBullCow;

interface
uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestBullCow = class(TObject) 
  public
    [Test]
    procedure MakeCorrectGuessInFirstTry;

    [Test]
    procedure MakeCorrectGuessInSecondTry;
  end;

implementation
uses SysUtils, uBullCow;

procedure TTestBullCow.MakeCorrectGuessInFirstTry;
var
  SecretValue: TSecretNumber;
  BullsAndCows: IBullsAndCows;
  guessResult: TGuessResult;
begin
  SetLength(SecretValue, 4);
  SecretValue[0] := 1;
  SecretValue[1] := 2;
  SecretValue[2] := 3;
  SecretValue[3] := 4;
  BullsAndCows := NewBullsAndCows('1234');
  guessResult := BullsAndCows.makeAGuess('1234');
  Assert.IsTrue(guessResult.Success);
  Assert.AreEqual(1, guessResult.Turns);
  Assert.AreEqual(SecretValue, guessResult.SecretNumber);
end;

procedure TTestBullCow.MakeCorrectGuessInSecondTry;
var
  SecretValue: TSecretNumber;
  BullsAndCows: IBullsAndCows;
  guessResult: TGuessResult;
begin
  SetLength(SecretValue, 4);
  SecretValue[0] := 1;
  SecretValue[1] := 2;
  SecretValue[2] := 3;
  SecretValue[3] := 4;
  BullsAndCows := NewBullsAndCows('1234');

  guessResult := BullsAndCows.makeAGuess('4231');
  Assert.IsFalse(guessResult.Success);
  Assert.AreEqual(1, guessResult.Turns);
  Assert.AreEqual(2, guessResult.GuessScore.Bulls);
  Assert.AreEqual(2, guessResult.GuessScore.Cows);

  guessResult := BullsAndCows.makeAGuess('1234');
  Assert.IsTrue(guessResult.Success);
  Assert.AreEqual(2, guessResult.Turns);
  Assert.AreEqual(SecretValue, guessResult.SecretNumber);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBullCow);
end.
