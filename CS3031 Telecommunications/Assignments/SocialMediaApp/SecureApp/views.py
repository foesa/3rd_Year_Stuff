
from django.views.generic import TemplateView
from django.shortcuts import render, redirect
from django.contrib.auth.models import User
from accounts.models import userMake
from .forms import HomeForm
from .models import Post, userFriends
from django.contrib.auth.decorators import login_required
class HomeView(TemplateView):
    template_name = 'home.html'

    def get(self, request):
        form = HomeForm()
        posts = Post.objects.all().order_by('-created')
        users = User.objects.exclude(id=request.user.id)
        friend = userFriends.objects.get(current_user=request.user)
        friends =  friend.users.all()
        args = {
            'form': form, 'posts': posts,'users': users, 'friends': friends
        }
        return render(request, self.template_name, args)

    def post(self, request):
        form = HomeForm(request.POST)
        if form.is_valid():
            post = form.save(commit=False)
            post.user = request.user
            post.save()

            text = form.cleaned_data['post']
            form = HomeForm()
            return redirect('home')

        args = {'form': form}
        return render(request, self.template_name, args)

def change_friends(request, operation, pk):
    friend = User.objects.get(pk=pk)
    if operation == 'add':
        userFriends.make_friend(request.user, friend)
    elif operation == 'remove':
        userFriends.lose_friend(request.user, friend)
    return redirect('home')