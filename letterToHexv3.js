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
    9: "09",
    ",": "2D",
    "?": "2C",
    "!": "2B",
    "-": "28",
};

////// intro text 1 - 751 buen numero para completar 1 pantalla con txt.
///const message = `During the 80s, the video store was created, and for a long time they offered entertainment by renting movies in a revolutionary technology called VHS. A decade later, renting movies was an everyday thing. The seemingly endless demand for VHS left movie store companies with no time to rewind their movies. This sparked protests and clashes for almost a year. To end this conflict, the pre-rewind law was created, forcing customers to rewind films before returning them. A note was added every time a customer did not rewind a movie, and on the third note, their video store card was removed. Many resisted returning their cards, so a new force was established to enforce this law ... the blade rewinders.`;
///// dialogue 1
// const message = `Houston, we have a problem! They were waiting for me! But I can see the vhs ... My precious ... I have to get my hands on that vhs. For the sake of my Saturday nights, I have to! Everything ends now! I swear on Keanu Reeves they are not going to get away with it! You against my Blade Rewinders, may the best nerd win!`;
///// end text
const message = 'no logro encontrarla en este lugar tan enorme, Por que elegi un lugar asi? Ah, si...soy un overkill... no puede ser! ya suena The 7th Element! Me estoy quedando sin tiempo, tengo que encontrarla ya!'

const toggleOn = true;
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
    // if extra or missing lines, toggle this code below
    ///////////
    if (toggleOn) {
        lineArray.forEach(element => {
            resultArray.push(element);
        });
    }
    ///////////
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
        stringHex += letters[resultMessage[i].toLowerCase()];
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