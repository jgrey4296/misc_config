package com.jgrey4296.twitter_bookmark

import android.app.ActivityManager
import android.app.Application
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.Button
import android.widget.TextView
import androidx.recyclerview.widget.RecyclerView

class TagRecyclerAdapter(private val dataSet: ArrayList<String>) : RecyclerView.Adapter<TagRecyclerAdapter.TagViewHolder>() {

    /**
     * Provide a reference to the type of views that you are using
     * (custom ViewHolder).
     */
    class TagViewHolder(view: View, adapter: TagRecyclerAdapter) : RecyclerView.ViewHolder(view) {
        val textView: TextView

        init {
            // Define click listener for the ViewHolder's View.
            textView = view.findViewById(R.id.tag_text)
            // add the remove click listener
            view.findViewById<Button>(R.id.tag_remove).setOnClickListener {
                adapter.dataSet.remove(textView.text.toString())
                adapter.notifyDataSetChanged()
            }
        }
    }

    // Create new views (invoked by the layout manager)
    override fun onCreateViewHolder(viewGroup: ViewGroup, viewType: Int): TagViewHolder {
        // Create a new view, which defines the UI of the list item
        val view = LayoutInflater.from(viewGroup.context)
                .inflate(R.layout.tag_view, viewGroup, false)

        return TagViewHolder(view, this)
    }

    // Replace the contents of a view (invoked by the layout manager)
    override fun onBindViewHolder(viewHolder: TagViewHolder, position: Int) {

        // Get element from your dataset at this position and replace the
        // contents of the view with that element
        viewHolder.textView.text = dataSet[position]
    }

    // Return the size of your dataset (invoked by the layout manager)
    override fun getItemCount() = dataSet.size

}
