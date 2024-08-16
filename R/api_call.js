function myfunc(input, model){
fetch('https://api.openai.com/v1/embeddings', {
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

    .then(response => response.json())
    .then(response => console.log(JSON.stringify(response)))
}

myfunc(input = "teacher", model = "text-embedding-ada-002", api_key = "")