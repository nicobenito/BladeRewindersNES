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
//const message = `in the 90s movies were the entertainment first choice. movies were stored on a revolutionary technology called the vhs. el aumento de demanda genero una necesidad imprevista. no era posible rebobinar las peliculas a tiempo. fue entonces cuando el comite de video stores determino la ley del pre rebobinado. cada cliente tenia que devolver las peliculas rebobinadas. 123fue entonces cuando el comite de video stores determino la ley del pre rebobinado. cada cliente tenia que devolver las peliculas rebobinadas. 321fue entonces cuando el comite de video stores determino la ley del pre rebobinado. cada cliente tenia que devolver las peliculas rebobinadas1. `;
///// dialogue 1
const message = `hey honey. i want to return the movie we saw yestarday. what do you mean you already returned it. do you rewinded it right. oh god i can not believed this.2hey honey. i want to return the movie we saw yestarday. what do you mean you already returned it. do you rewinded it right. oh god i can not believed this.3hey honey. i want to return the movie we saw yestarday. what do you mean you already returned it. do you rewinded it right. oh god i can not believed this.4hey honey. i want to return the movie we saw yestarday. what do you mean you already returned it. do you rewinded it right. oh god i can not believed this.`;
const txtLimit = 170; // dialogue limit

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