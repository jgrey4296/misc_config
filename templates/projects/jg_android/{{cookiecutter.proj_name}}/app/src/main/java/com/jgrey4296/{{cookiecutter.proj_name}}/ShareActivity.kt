package com.jgrey4296.twitter_bookmark

import java.io.File
import android.os.Bundle
import com.google.android.material.snackbar.Snackbar
import androidx.appcompat.app.AppCompatActivity
import com.jgrey4296.twitter_bookmark.databinding.ActivityShareBinding
import android.util.Log
import android.widget.TextView
import androidx.appcompat.widget.SearchView
import androidx.recyclerview.widget.RecyclerView
import com.google.android.material.snackbar.Snackbar.make
import com.jgrey4296.twitter_bookmark.TagRecyclerAdapter


class ShareActivity : AppCompatActivity(), SearchView.OnQueryTextListener {

    private lateinit var binding: ActivityShareBinding
    private val tags : ArrayList<String> = ArrayList<String>()

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        Log.i("TwitterBookmark", "---- Share Activity Start")

        binding = ActivityShareBinding.inflate(layoutInflater)
        setContentView(binding.root)

        binding.tagRecycler.adapter = TagRecyclerAdapter(tags)


        val tweets_dir = File(filesDir, "tweets")
        tweets_dir.mkdir()

        val tweets_file = tweets_dir.resolve(getString(R.string.tweets_file))
        tweets_file.createNewFile()
        val tweet = intent.clipData?.getItemAt(0)?.text?.toString() ?: "None Extracted"
        binding.shareText.text = tweet

        binding.shareTweetButton.setOnClickListener {
            // Write to File
            val tags : String = this.tags.joinToString(",")
            tweets_file.appendText("$tweet _:_ $tags\n")
            // Display confirmation
            make(binding.root, "File Written", Snackbar.LENGTH_LONG)
                .setAction("Action", null).show()
            Log.i("TwitterBookmark", "Tweet Added")
            // TODO check size, if too much, notify
            this.finish()
        }

        binding.tagSearchBar.setOnQueryTextListener(this)
        // Debug info
        Log.i("TwitterBookmark", "Files: " + fileList().fold("") { a, b -> "$a $b"})
        Log.i("TwitterBookmark", "Tweet: $tweet")
        Log.i("TwitterBookmark", "Read File Contents: $tweet")

        // add_tweet(tweet)

        // Create and run a send intent to dropbox

        Log.i("TwitterBookmark", "---- Share Activity End")
    }

    override fun onQueryTextChange (query: String) : Boolean {
        Log.i("TwitterBookmark", "$query")
        return false
    }

    override fun onQueryTextSubmit (query: String) : Boolean {
        Log.i("TwitterBookmark", "Subtmitted: $query")
        this.tags.add(query)
        binding.tagRecycler.adapter?.notifyDataSetChanged()
        this.binding.tagSearchBar.setQuery("", false)
        return false
    }

    fun removeTag (tag : String) {
        Log.i("TwitterBookmark", "Removing $tag")
        this.tags.remove(tag)
        binding.tagRecycler.adapter?.notifyDataSetChanged()
    }

}
