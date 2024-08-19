async function myfunc(input, model){
    if (typeof input != 'string'){
        throw new Error("Inputs must be a character vector of terms to get embeddings for.");
    }

    if (!api_key || api_key ==""){
        throw new Error("API key is missing. Please set the OPENAI_API_KEY environment variable.");
    }

    try{
    const res = await fetch('https://api.openai.com/v1/embeddings', {
    method: 'POST', 
    headers: {
        'Accept': 'application.json',
        'Content-Type': 'application/json',
        "Authorization": "Bearer " + api_key
    },

    // body/input
    body: JSON.stringify({
        model: model,
        input: input
    }) 
})

// request check
console.log(res.status)
if(res.status !== 200){
    // log out failed request
    console.error('Failed request with status:', res.status);
} else{
    const develop = await res.json();
    console.log(develop);
}
} catch (error){
    console.error('Fetch error:', error.message);
}
}



myfunc(input = "test_data.csv", model = "text-embedding-ada-002", api_key = "")