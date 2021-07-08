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
        describe "nextHeadPosition" $ do
            let snakeState = mkSnakePosition [(5, 5), (5, 6), (5, 7)]
            it "handle left correctly" $ do
                let newHeadPosition = nextHeadPosition Snake.Left snakeState
                newHeadPosition `shouldBe` mkLogicCoord 4 5
            it "handle right correctly" $ do
                let newHeadPosition = nextHeadPosition Snake.Right snakeState
                newHeadPosition `shouldBe` mkLogicCoord 6 5
            it "handle up correctly" $ do
                let newHeadPosition = nextHeadPosition Snake.Up snakeState
                newHeadPosition `shouldBe` mkLogicCoord 5 4
            it "handle up correctly" $ do
                let newHeadPosition = nextHeadPosition Snake.Down snakeState
                newHeadPosition `shouldBe` mkLogicCoord 5 6

        describe "moveSnake" $ do
            describe "handling moves without food" $ do
                it "could move left correctly" $ do
                    let snakeState = mkSnakePosition [(5, 5), (5, 6), (5, 7)]
                    let gameState = testGameState { snakePosition = snakeState }
                    let nextPosition = snakePosition <$> moveSnake Snake.Left gameState
                    nextPosition `shouldBe` Prelude.Right (mkSnakePosition [(4, 5), (5, 5), (5, 6)])

                it "could move right correctly" $ do
                    let snakeState = mkSnakePosition [(5, 5), (5, 6), (5, 7)]
                    let gameState = testGameState { snakePosition = snakeState }
                    let nextPosition = snakePosition <$> moveSnake Snake.Right gameState
                    nextPosition `shouldBe` Prelude.Right (mkSnakePosition [(6, 5), (5, 5), (5, 6)])

                it "could move up correctly" $ do
                    let snakeState = mkSnakePosition [(5, 5), (5, 6), (5, 7)]
                    let gameState = testGameState { snakePosition = snakeState }
                    let nextPosition = snakePosition <$> moveSnake Snake.Up gameState
                    nextPosition `shouldBe` Prelude.Right (mkSnakePosition [(5, 4), (5, 5), (5, 6)])

                it "could move down correctly" $ do
                    let snakeState = mkSnakePosition [(5, 5), (5, 4), (5, 3)]
                    let gameState = testGameState { snakePosition = snakeState }
                    let nextPosition = snakePosition <$> moveSnake Snake.Down gameState
                    nextPosition `shouldBe` Prelude.Right (mkSnakePosition [(5, 6), (5, 5), (5, 4)])

            it "could detect self-eating" $ do
                let snakeState = mkSnakePosition [(2, 2), (2, 3), (1, 3), (1, 2), (1, 1)]
                let gameState = testGameState { snakePosition = snakeState }
                let nextPosition = snakePosition <$> moveSnake Snake.Left gameState
                nextPosition `shouldBe` Prelude.Left CellTaken

            describe "boundary hit detection" $ do
                it "could detect top boundary hit when moving up" $ do
                    let snakeState = mkSnakePosition [(1, 1), (2, 1), (3, 1)]
                    let gameState = testGameState { snakePosition = snakeState }
                    let nextGameState = moveSnake Snake.Up gameState
                    nextGameState `shouldBe` Prelude.Left (BoundaryHit $ mkLogicCoord 1 0)

                it "could detect left boundary hit" $ do 
                    let snakeState = mkSnakePosition [(1, 1), (2, 1), (3, 1)]
                    let gameState = testGameState { snakePosition = snakeState }
                    let nextGameState = moveSnake Snake.Left gameState
                    nextGameState `shouldBe` Prelude.Left (BoundaryHit $ mkLogicCoord 0 1)

                it "could detect bottom boundary hit" $ do 
                    let snakeState = mkSnakePosition [(9, 9), (9, 8), (9, 7)]
                    let gameState = testGameState { snakePosition = snakeState }
                    let nextGameState = moveSnake Snake.Down gameState
                    nextGameState `shouldBe` Prelude.Left (BoundaryHit $ mkLogicCoord 9 10)

                it "could detect right boundary hit" $ do 
                    let snakeState = mkSnakePosition [(9, 9), (9, 8), (9, 7)]
                    let gameState = testGameState { snakePosition = snakeState }
                    let nextGameState = moveSnake Snake.Right gameState
                    nextGameState `shouldBe` Prelude.Left (BoundaryHit $ mkLogicCoord 10 9)

            it "could handle eating food" $ do
                let snakePositionsBefore = [(2, 2), (2, 3), (1, 3), (1, 2), (1, 1)]
                let foodPositions = [(2, 1)]
                let gameState = testGameState {
                    snakePosition = mkSnakePosition snakePositionsBefore,
                    foodPositions = map (uncurry mkLogicCoord) foodPositions
                }
                let newGameState = moveSnake Snake.Up gameState
                let newSnakePosition = snakePosition <$> newGameState
                let newFoodPosition = Snake.foodPositions <$> newGameState

                let expectedSnakePosition = mkSnakePosition $ foodPositions ++ snakePositionsBefore
                newSnakePosition `shouldBe` Prelude.Right expectedSnakePosition
                newFoodPosition `shouldBe` Prelude.Right []


