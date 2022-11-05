// Beginner

// Part 1
// Create a function to reverse any word that you type in. This can be typed into either an input box or an alert box, and then print the result in a box or on the webpage.


// Create Function to reverse user input word
function ReverseString(str) {
    
    // Check input
    if(!str || str.length < 2 || typeof str!== 'string') {
    return 'Invalid input. Please enter a word or phrase.';
}
    // Reverse Word
    let reverseWord = str.split('').reverse().join('')

    return reverseWord
}

// Declare necessary variables related to user input
const text1 = document.getElementById("Text1")
const button1 = document.getElementById("Button1")
const output1 = document.getElementById("Output1")

// Create function to display reversed word in desired output
function ReverseUserWord() {
    output1.innerHTML = ReverseString(text1.value)
}

// add an event listener to run program
button1.addEventListener('click', ReverseUserWord)


// Part 2
// Create a function that takes an input number, and prints a table with the first 20 multiples of the number, in order 5x4

// Declare necessary variables related to user input
const text2 = document.getElementById("Text2")
const button2 = document.getElementById("Button2")
const table1 = document.getElementById("Table1")

// Create function to get multiples
function GetMultiples(userNum){
    let multiples = []
    for(let i=1; i<21;i++){
        
        multiples[i-1] = userNum * i

    }
    return multiples
}

// Create function to store multiples in html table
function PopulateTable(){

    array = GetMultiples(text2.value)

    let row1 = array.slice(0,4)
    let row2 = array.slice(4,8)
    let row3 = array.slice(8,12)
    let row4 = array.slice(12,16)
    let row5 = array.slice(16,20)
    let multiDimArray = [row1, row2, row3, row4, row5]

    for(let i = 0; i < table1.rows.length; i++){
        for(let j = 0; j < table1.rows[i].cells.length; j++){
            table1.rows[i].cells[j].innerHTML = multiDimArray[i][j]
        }
    }
}

// add an event listener to run program
button2.addEventListener('click', PopulateTable)

