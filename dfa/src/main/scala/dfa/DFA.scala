package dfa // leave this line in the file

case class State(label: String)
case class Transition(from: State, to: State, symbol: Char)

class DFA(val states: Set[State], val transitions: Set[Transition], val start: State, val accept: Set[State]):
    def accepts(input: String) = {
        // If the input is empty, directly check the start state.
        if input == "" then accept.contains(start)
        // Otherwise, continue to go through the input.
        else
            var currentState = start
            var content = input
            var symbol = content.head
            
            while (content != "") {
                // Get the state of the transition that goes from the currentState with symbol.
                currentState = transitions.filter((t: Transition) => (t.from == currentState) && (t.symbol == symbol)).head.to

                // Update the current state and remove the first char from the content. 
                symbol = content.head
                content = content.tail
            }
            
            // Returns true of the current state when the content is empty is an accepting state.
            accept.contains(currentState)
    }
