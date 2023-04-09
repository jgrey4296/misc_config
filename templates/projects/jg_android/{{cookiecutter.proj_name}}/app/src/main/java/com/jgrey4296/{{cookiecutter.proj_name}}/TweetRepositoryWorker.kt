package com.jgrey4296.twitter_bookmark

import java.io.File
import android.util.Log
import android.content.Context
import androidx.work.WorkerParameters
import androidx.work.CoroutineWorker

class TweetRepositoryWorker(context: Context, params: WorkerParameters) : CoroutineWorker(context, params) {

    fun export_tweets(): String {
        // // Export all internal tweets as a string
        // val file = File(filesDir, tweet_file)
        // Log.i("TweetBookmarkRepo", file.toString())
        // return file.toString()
        return "None"
    }

    override suspend fun doWork(): Result {
        // try {
        //     val data = export_tweets()
        //     // TODO Then write it to dropbox
        //     //
            return Result.success()

        // } catch (error : Throwable) {
        //     Result.failure()
        // }
    }

}
