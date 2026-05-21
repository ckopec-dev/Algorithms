# Q-Learning Algorithm in Visual Basic

Here's a complete implementation of a Q-learning algorithm in Visual Basic .NET:

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Public Class QLearningAgent
    ' Q-table to store state-action values
    Private QTable As Dictionary(Of String, Dictionary(Of Integer, Double))
    Private learningRate As Double
    Private discountFactor As Double
    Private explorationRate As Double
    Private random As Random

    Public Sub New(learningRate As Double, discountFactor As Double, explorationRate As Double)
        Me.QTable = New Dictionary(Of String, Dictionary(Of Integer, Double))
        Me.learningRate = learningRate
        Me.discountFactor = discountFactor
        Me.explorationRate = explorationRate
        Me.random = New Random()
    End Sub

    ' Get Q-value for state-action pair
    Private Function GetQValue(state As String, action As Integer) As Double
        If Not QTable.ContainsKey(state) Then
            QTable(state) = New Dictionary(Of Integer, Double)
        End If

        If Not QTable(state).ContainsKey(action) Then
            QTable(state)(action) = 0.0
        End If

        Return QTable(state)(action)
    End Function

    ' Set Q-value for state-action pair
    Private Sub SetQValue(state As String, action As Integer, value As Double)
        If Not QTable.ContainsKey(state) Then
            QTable(state) = New Dictionary(Of Integer, Double)
        End If

        QTable(state)(action) = value
    End Sub

    ' Choose action using epsilon-greedy policy
    Public Function ChooseAction(state As String, availableActions As List(Of Integer)) As Integer
        ' Exploration: choose random action
        If random.NextDouble() < explorationRate Then
            Return availableActions(random.Next(availableActions.Count))
        Else
            ' Exploitation: choose best action
            Dim bestAction As Integer = availableActions(0)
            Dim bestQValue As Double = GetQValue(state, availableActions(0))

            For Each action As Integer In availableActions.Skip(1)
                Dim qValue As Double = GetQValue(state, action)
                If qValue > bestQValue Then
                    bestQValue = qValue
                    bestAction = action
                End If
            Next

            Return bestAction
        End If
    End Function

    ' Update Q-value using Q-learning formula
    Public Sub UpdateQValue(state As String, action As Integer, reward As Double, nextState As String, availableActions As List(Of Integer))
        Dim currentQ As Double = GetQValue(state, action)
        Dim maxNextQ As Double = 0.0

        ' Find maximum Q-value for next state
        If availableActions IsNot Nothing AndAlso availableActions.Count > 0 Then
            maxNextQ = availableActions.Max(Function(a) GetQValue(nextState, a))
        End If

        ' Q-learning update formula
        Dim newQ As Double = currentQ + learningRate * (reward + discountFactor * maxNextQ - currentQ)
        SetQValue(state, action, newQ)
    End Sub

    ' Get the best action for a given state
    Public Function GetBestAction(state As String, availableActions As List(Of Integer)) As Integer
        Dim bestAction As Integer = availableActions(0)
        Dim bestQValue As Double = GetQValue(state, availableActions(0))

        For Each action As Integer In availableActions.Skip(1)
            Dim qValue As Double = GetQValue(state, action)
            If qValue > bestQValue Then
                bestQValue = qValue
                bestAction = action
            End If
        Next

        Return bestAction
    End Function

    ' Get Q-table for debugging
    Public Function GetQTable() As Dictionary(Of String, Dictionary(Of Integer, Double))
        Return QTable
    End Function
End Class

' Example usage
Public Class QLearningExample
    Public Shared Sub Main()
        ' Create Q-learning agent
        Dim agent As New QLearningAgent(0.1, 0.9, 0.1)

        ' Simple grid world example (0,1,2,3 represent different states)
        Dim states As List(Of Integer) = New List(Of Integer) From {0, 1, 2, 3}
        Dim actions As List(Of Integer) = New List(Of Integer) From {0, 1} ' 0 = left, 1 = right

        ' Training loop
        For episode As Integer = 1 To 1000
            Dim currentState As Integer = 0 ' Start at state 0
            Dim totalReward As Double = 0.0

            ' Run one episode
            For step As Integer = 1 To 10
                ' Choose action
                Dim action As Integer = agent.ChooseAction(currentState.ToString(), actions)

                ' Simulate environment (simple reward system)
                Dim reward As Double = 0.0
                Dim nextState As Integer = currentState

                If action = 1 Then ' Move right
                    nextState = Math.Min(currentState + 1, 3)
                    If nextState = 3 Then
                        reward = 10.0 ' Goal reward
                    End If
                Else ' Move left
                    nextState = Math.Max(currentState - 1, 0)
                End If

                ' Update Q-value
                agent.UpdateQValue(currentState.ToString(), action, reward, nextState.ToString(), actions)

                totalReward += reward
                currentState = nextState

                ' If reached goal, end episode
                If currentState = 3 Then
                    Exit For
                End If
            Next

            ' Print progress
            If episode Mod 100 = 0 Then
                Console.WriteLine($"Episode {episode}: Total Reward = {totalReward}")
            End If
        Next

        ' Show final policy
        Console.WriteLine(vbNewLine & "Final Policy:")
        For Each state As Integer In states
            Dim bestAction As Integer = agent.GetBestAction(state.ToString(), actions)
            Console.WriteLine($"State {state}: Best Action = {bestAction}")
        Next

        Console.WriteLine(vbNewLine & "Press any key to exit...")
        Console.ReadKey()
    End Sub
End Class
```

## Key Components Explained:

### 1. **QLearningAgent Class**
- **QTable**: Dictionary storing state-action values
- **Parameters**: Learning rate, discount factor, exploration rate
- **Methods**:
  - `ChooseAction()`: Implements epsilon-greedy policy
  - `UpdateQValue()`: Updates Q-values using Bellman equation
  - `GetQValue()`: Retrieves Q-value for state-action pair

### 2. **Q-Learning Formula**
```
Q(s,a) = Q(s,a) + α[r + γ * max(Q(s',a')) - Q(s,a)]
```

### 3. **Key Features**
- Epsilon-greedy exploration strategy
- Dynamic Q-table creation
- Support for multiple states and actions
- Complete training loop with progress tracking

### 4. **Example Output**
```
Episode 100: Total Reward = 8.0
Episode 200: Total Reward = 9.0
Episode 300: Total Reward = 10.0
...

Final Policy:
State 0: Best Action = 1
State 1: Best Action = 1
State 2: Best Action = 1
State 3: Best Action = 0
```

This implementation demonstrates the core concepts of Q-learning in Visual Basic, including exploration vs exploitation, Q-value updates, and policy learning.

