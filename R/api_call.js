async function myfunc(input, model, api_key) {
    // Input validation
    if (typeof input !== 'string') {
        throw new Error("Inputs must be a character vector of terms to get embeddings for.");
    }

    if (!api_key || api_key === "") {
        throw new Error("API key is missing. Please set the OPENAI_API_KEY environment variable.");
    }

    try {
        const response = await fetch('https://api.openai.com/v1/embeddings', {
            method: 'POST',
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json',
                "Authorization": `Bearer ${api_key}`
            },
            body: JSON.stringify({
                model: model,
                input: input
            })
        });

        // Check response status
        console.log(response.status);
        if (response.status !== 200) {
            // Log out failed request
            console.error('Failed request with status:', response.status);
        } else {
            // Parse the JSON response
            const data = await response.json();

            // Log and handle data
            console.log(data);

            // If running in Node.js, write data to a JSON file
            const fs = require('fs');
            fs.writeFileSync('data.json', JSON.stringify(data));
        }
    } catch (error) {
        // Handle fetch errors
        console.error('Fetch error:', error.message);
    }
}