package com.jgrey4296.twitter_bookmark

import android.content.Intent
import android.os.Bundle
import androidx.appcompat.app.AppCompatActivity
import android.util.Log
import androidx.core.content.FileProvider.getUriForFile
import java.io.File
import com.jgrey4296.twitter_bookmark.databinding.ActivityMainBinding
import java.io.FileInputStream

class MainActivity : AppCompatActivity() {

    private lateinit var binding: ActivityMainBinding

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        Log.i("TwitterBookmark", "--------")
        binding = ActivityMainBinding.inflate(layoutInflater)
        setContentView(binding.root)
        val file_list = File(filesDir, "tweets").list()?.fold("") { some, text -> "$some, $text"}
        Log.i("TwitterBookmark", "File List: $file_list")

        displayCount()
        val tweets_file = File(filesDir, "tweets").resolve(getString(R.string.tweets_file))

        // TODO handle share button
        binding.exportTweetsButton.setOnClickListener {
            Log.i("TwitterBookmark", "Sharing")
            Log.i("TwitterBookmark", tweets_file.toString())
            val contentUri = getUriForFile(applicationContext, "com.jgrey4296.twitter_bookmark.fileprovider", tweets_file)

            val sendIntent: Intent = Intent().apply {
                action = Intent.ACTION_SEND
                data = contentUri
                type = "text/plain"
                putExtra(Intent.EXTRA_STREAM, contentUri)
                setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION)
            }
            startActivity(sendIntent)

            // Then clear the send texts
        }

        binding.clearButton.setOnClickListener {
            tweets_file.delete()
            tweets_file.createNewFile()
            displayCount()
        }

        Log.i("TwitterBookmark", "--------")
    }

    override fun onResume() {
        super.onResume()
        displayCount()
    }

    override fun onRestart() {
        super.onRestart()
        displayCount()
    }

    private fun displayCount () {
        File(filesDir, "tweets").mkdir()
        val tweets_file = File(filesDir, "tweets").resolve(getString(R.string.tweets_file))
        tweets_file.createNewFile()
        val lines = FileInputStream(tweets_file).bufferedReader().readLines()
        val count = lines.count()

        binding.tweetLines.text = "Tweets Collected: $count"
    }

}
