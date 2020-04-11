from cryptography.fernet import Fernet
from django.views.generic import TemplateView
from django.shortcuts import render, redirect
from django.contrib.auth.models import User
from .forms import HomeForm
from .models import Post, userFriends
from django.contrib.auth.decorators import login_required
class HomeView(TemplateView):
    template_name = 'home.html'

    def get(self, request):
        k = Fernet.generate_key()
        crypter = Fernet(k)
        form = HomeForm()
        posts = Post.objects.all().order_by('-created')
        users = User.objects.exclude(id=request.user.id)
        friend = userFriends.objects.filter(current_user=request.user)
        friends = None
        if(len(friend) > 0):
            friends = userFriends.objects.get(current_user=request.user).users.all()
        for i in posts:
            otherUser = i.user
            otherUserlist = userFriends.objects.filter(current_user=otherUser)
            if(len(otherUserlist) > 0):
                ousers = userFriends.objects.get(current_user=otherUser).users.all().filter(id=request.user.id).exists()
            print(otherUserlist)
            if(not i.user.id == request.user.id):
                if friends is None or not friends.filter(id=i.user.id).exists() or not ousers:
                    encrypted = crypter.encrypt(i.post.encode())
                    i.post = encrypted
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

def change_friendsr(request, operation, pk):
    friend = User.objects.get(pk=pk)
    if operation == 'add':
        userFriends.make_friend(request.user, friend)
    elif operation == 'remove':
        userFriends.lose_friend(request.user, friend)
    return redirect('view_profile_with_pk')