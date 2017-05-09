SELECT COALESCE(SUM(x.baseScore + x.bonus),0)
FROM (SELECT a.frame, a.baseScore, ballsThrown, bonus1.pins b1, bonus2.pins b2, bonus3.pins b3,
            CASE
                WHEN a.baseScore = 10 AND a.ballsThrown = 2 THEN COALESCE(bonus1.pins, 0)
                WHEN a.baseScore = 10 AND a.ballsThrown = 1 THEN COALESCE(bonus1.pins, 0) + COALESCE(bonus2.pins, bonus3.pins, 0)
                ELSE 0
            END bonus
      FROM (SELECT frame, SUM(pins) baseScore, COUNT(pins) ballsThrown
            FROM bowling
            GROUP BY frame) a
      LEFT JOIN bowling bonus1 ON bonus1.frame = a.frame+1 AND bonus1.ball = 1
      LEFT JOIN bowling bonus2 ON bonus2.frame = a.frame+1 AND bonus2.ball = 2
      LEFT JOIN bowling bonus3 ON bonus3.frame = a.frame+2 AND bonus3.ball = 1) x
