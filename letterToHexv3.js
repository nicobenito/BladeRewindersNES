const letters = {
    a: "0A",
    b: "0B",
    c: "0C",
    d: "0D",
    e: "0E",
    f: "0F",
    g: "10",
    h: "11",
    i: "12",
    j: "13",
    k: "14",
    l: "15",
    m: "16",
    n: "17",
    o: "18",
    p: "19",
    q: "1A",
    r: "1B",
    s: "1C",
    t: "1D",
    u: "1E",
    v: "1F",
    w: "20",
    x: "21",
    y: "22",
    z: "23",
    _: "24",
    ".": "2F",
    0: "00",
    1: "01",
    2: "02",
    3: "03",
    4: "04",
    5: "05",
    6: "06",
    7: "07",
    8: "08",
    9: "09"
};

////// intro text 1 - 751 buen numero para completar 1 pantalla con txt.
// const message = `during the 80s the video store was created and for a long time they offered entertainment by renting movies in a revolutionary technology called vhs. a decade later renting movies was an everyday thing. the seemingly endless demand for vhs left video store companies with no time to rewind their movies. this sparked protests and riots for almost a year. to end this conflict the pre rewind law was created forcing customers to rewind films before returning them. a note was added every time a customer did not rewind a movie and on the third note their video store card was removed. many resisted returning their cards so a new force was established to enforce this law ... the blade rewinders.`;
///// dialogue 1
const message = `hey honey i am going shopping and i thought about returning the movie we saw yesterday but i cant find it where is it ...... oh you already returned it very good. one question you rewound it right ...... when did you tell me to do it i didnt hear you say it. if i dont answer you its because i didnt listen to you. ...... well its not a problem its our second fault we just have to be more careful next time ...... our what i cant believe its the third. no no no. the blade rewinders are going to be here any minute. ...... wait i hear something outside ... dont worry i am going to make it to the video store without getting caught and rewind that movie`;
const txtLimit = 170; // dialogue limit
// const txtLimit = 752; // full screen txt limit

const stringToHex = () => {
    /*
    defino array final
    defino array de renglon
    defino largo del renglon
    spliteo por espacios
    recorro array de palabras
    defino contador de renglon
    largo de palabra + espacio(1) + contador <= largo renglon?
        push de espacio + palabra al array de renglon
        sumo length + espacio al contador
    sino
        vuelco array de renglon en array final
        agrego espacios = largo - contador + 1 espacio final
        contador = 0
        agrego espacio + palabra actual al array de renglon
        sumo length + espacio al contador
        --
    creo un string total con el array final.
    reemplazo cada caracter por $+hexa q correponda.
    cuenta el length, muestro el total.

    -------- txt limit
    defino limite
    idea 1:
    -divido el txt por el limite
    -proceso ese txt con los espacios
    -al resultado le agrego los espacios faltantes
    ? como evito cortar palabras?
    
    idea2:
    -

     */
    let stringHex = "";
    const resultArray = [];
    let lineArray = [];
    const lineTotal = 32;
    let lineCounter = 0;
    let limitCounter = 0;
    let pageCounter = 1;
    const words = message.split(" ");
    words.forEach(word => {
        if (word.length + limitCounter > txtLimit) {
            const letters1 = resultArray.join('').length;
            const resultDone = txtLimit * pageCounter - letters1;
            for (let i = 0; i < resultDone; i++) {
                resultArray.push("_");
            };
            const letters = resultArray.join('').length;
            lineCounter = lineArray.join('').length;
            limitCounter = 0;
            pageCounter ++;
        }
        if (word.length + lineCounter + 1 <= lineTotal) {
            lineArray.push("_");
            lineArray.push(word);
            lineCounter = lineCounter + word.length + 1;
            limitCounter = limitCounter + word.length + 1;
        } else {
            lineArray.forEach(element => {
                resultArray.push(element);
            });
            for (let i = 0; i < lineTotal - lineCounter; i++) {
                resultArray.push("_");
                limitCounter ++;
            };
            if (word == words[words.length - 1]) {
                resultArray.push("_");
                resultArray.push(word);
            } else {
                lineArray = [];
                lineCounter = 0;
                lineArray.push("_");
                lineArray.push(word);
                lineCounter = lineCounter + word.length + 1;
                limitCounter = limitCounter + word.length + 1;
            }            
        }
    });

    lineArray.forEach(element => {
        resultArray.push(element);
    });

    let resultMessage = "";
    resultArray.forEach(element => {
        resultMessage += element;
    });

    console.log(resultMessage);
    for (let i = 0; i < resultMessage.length; i++) {
        // if (i % 20 === 0  ) {
        //     stringHex += "\n.db ";
        // }
        stringHex += '$'
        stringHex += letters[resultMessage[i]];
        stringHex += ', '
    }
    // console.log(stringHex);
    console.log(resultMessage.length);
    console.log(stringHex.split(", ").length.toString(16));
    const resultHex = stringHex.split(",");
    let toPrint = '';
    for (let i = 0; i < resultHex.length; i++) {
        if (i % 20 === 0  ) {
            toPrint += "\n.db";
        }
        toPrint += resultHex[i];
        toPrint+= ',';
    }
    console.log(toPrint);
}

stringToHex();