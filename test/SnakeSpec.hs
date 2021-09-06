module SnakeSpec where

import Test.Hspec
import Snake

mkSnakePosition :: [(Int, Int)] -> SnakePosition
mkSnakePosition coordsList = SnakePosition $ map (uncurry mkLogicCoord) coordsList

snakeSpec :: Spec
snakeSpec =
    describe "Snake" $ do
        let testGameState = GameState {
            gameDimensions = mkRectangle (0, 0) (10, 10),
            snakePosition = mkSnakePosition [],
            headDirection = Snake.Up,
            foodPositions = [],
            ticksPassed = 0
        }

        describe "moveSnake" $ do
            describe "handling moves without food" $ do
                it "could move left correctly" $ do
                    let snakeState = mkSnakePosition [(5, 5), (5, 6), (5, 7)]
                    let gameState = testGameState { snakePosition = snakeState, headDirection = Snake.Left }
                    let nextPosition = snakePosition <$> moveSnake gameState
                    nextPosition `shouldBe` Prelude.Right (mkSnakePosition [(4, 5), (5, 5), (5, 6)])

                it "could move right correctly" $ do
                    let snakeState = mkSnakePosition [(5, 5), (5, 6), (5, 7)]
                    let gameState = testGameState { snakePosition = snakeState, headDirection = Snake.Right }
                    let nextPosition = snakePosition <$> moveSnake gameState
                    nextPosition `shouldBe` Prelude.Right (mkSnakePosition [(6, 5), (5, 5), (5, 6)])

                it "could move up correctly" $ do
                    let snakeState = mkSnakePosition [(5, 5), (5, 6), (5, 7)]
                    let gameState = testGameState { snakePosition = snakeState, headDirection = Snake.Up }
                    let nextPosition = snakePosition <$> moveSnake gameState
                    nextPosition `shouldBe` Prelude.Right (mkSnakePosition [(5, 4), (5, 5), (5, 6)])

                it "could move down correctly" $ do
                    let snakeState = mkSnakePosition [(5, 5), (5, 4), (5, 3)]
                    let gameState = testGameState { snakePosition = snakeState, headDirection = Snake.Down }
                    let nextPosition = snakePosition <$> moveSnake gameState
                    nextPosition `shouldBe` Prelude.Right (mkSnakePosition [(5, 6), (5, 5), (5, 4)])

            it "could detect self-eating" $ do
                let snakeState = mkSnakePosition [(2, 2), (2, 3), (1, 3), (1, 2), (1, 1)]
                let gameState = testGameState { snakePosition = snakeState, headDirection = Snake.Left }
                let nextPosition = snakePosition <$> moveSnake gameState
                nextPosition `shouldBe` Prelude.Left CellTaken

            describe "boundary hit detection" $ do
                it "could detect top boundary hit when moving up" $ do
                    let snakeState = mkSnakePosition [(1, 1), (2, 1), (3, 1)]
                    let gameState = testGameState { snakePosition = snakeState, headDirection = Snake.Up }
                    let nextGameState = moveSnake gameState
                    nextGameState `shouldBe` Prelude.Left (BoundaryHit $ mkLogicCoord 1 0)

                it "could detect left boundary hit" $ do
                    let snakeState = mkSnakePosition [(1, 1), (2, 1), (3, 1)]
                    let gameState = testGameState { snakePosition = snakeState, headDirection = Snake.Left }
                    let nextGameState = moveSnake gameState
                    nextGameState `shouldBe` Prelude.Left (BoundaryHit $ mkLogicCoord 0 1)

                it "could detect bottom boundary hit" $ do
                    let snakeState = mkSnakePosition [(9, 9), (9, 8), (9, 7)]
                    let gameState = testGameState { snakePosition = snakeState, headDirection = Snake.Down }
                    let nextGameState = moveSnake gameState
                    nextGameState `shouldBe` Prelude.Left (BoundaryHit $ mkLogicCoord 9 10)

                it "could detect right boundary hit" $ do
                    let snakeState = mkSnakePosition [(9, 9), (9, 8), (9, 7)]
                    let gameState = testGameState { snakePosition = snakeState, headDirection = Snake.Right }
                    let nextGameState = moveSnake gameState
                    nextGameState `shouldBe` Prelude.Left (BoundaryHit $ mkLogicCoord 10 9)

            it "could handle eating food" $ do
                let snakePositionsBefore = [(2, 2), (2, 3), (1, 3), (1, 2), (1, 1)]
                let foodPositions = [(2, 1)]
                let gameState = testGameState {
                    snakePosition = mkSnakePosition snakePositionsBefore,
                    foodPositions = map (uncurry mkLogicCoord) foodPositions,
                    headDirection = Snake.Up
                }
                let newGameState = moveSnake gameState
                let newSnakePosition = snakePosition <$> newGameState
                let newFoodPosition = Snake.foodPositions <$> newGameState

                let expectedSnakePosition = mkSnakePosition $ foodPositions ++ snakePositionsBefore
                newSnakePosition `shouldBe` Prelude.Right expectedSnakePosition
                newFoodPosition `shouldBe` Prelude.Right []

        describe "handleTick" $ do
            it "adds new tick to the total count" $ do
                let newGameState = handleTick testGameState
                newGameState `shouldBe` testGameState { ticksPassed = 1 + ticksPassed testGameState}

